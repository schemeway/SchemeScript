/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import gnu.mapping.*;

import java.net.*;
import java.util.*;
import java.util.List;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.ui.actions.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.Assert;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.*;
import org.eclipse.jface.util.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.*;
import org.eclipse.ui.part.*;
import org.eclipse.ui.texteditor.*;
import org.eclipse.ui.views.contentoutline.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.debug.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.autoedits.*;
import org.schemeway.plugins.schemescript.editor.outline.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.interpreter.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeEditor extends TextEditor {
    public final static String ID = "org.schemeway.plugins.schemescript.editor.SchemeEditor";

    private static ISymbolDictionary mDictionary;
    
    private SexpNavigator mNavigator;
    private PaintManager mPaintManager;
    private SchemeParenthesisPainter mParenPainter;
    private ISchemeOutlinePage mOutlinePage;
    private IToggleBreakpointsTarget mBreakpointTarget;

    private IAutoEditStrategy mQuoteInserter;
    private IAutoEditStrategy mMatchinDelimiterInserter;
    private IAutoEditStrategy mSexpDeleter;
    private IAutoEditStrategy mStringDeleter;
    private IAutoEditStrategy mCommentDeleter;
    
    private static List sSaveHooks = new ArrayList(); 

    public SchemeEditor() {
        super();
        SchemeTextTools textTools = SchemeScriptPlugin.getDefault().getTextTools();
        setSourceViewerConfiguration(new SchemeConfiguration(textTools, this));
        IPropertyChangeListener listener = new IPropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent event)
            {
                handlePreferenceStoreChanged(event);
            }
        };
        SchemeScriptPlugin.getDefault().getPreferenceStore().addPropertyChangeListener(listener);
    }

    public void dispose() {
        super.dispose();
    }

    //
    //// ContentOutlinePage support
    //

    public Object getAdapter(Class adapter) {
        if (IContentOutlinePage.class.equals(adapter)) {
            if (mOutlinePage == null) {
                mOutlinePage = createOutlinePage();
            }
            return mOutlinePage;
        }
        if (IToggleBreakpointsTarget.class.equals(adapter)) {
            return createToggleBreakpointsTarget();
        }
        return super.getAdapter(adapter);
    }

    protected ISchemeOutlinePage createOutlinePage() {
        return new SchemeOutlinePage(this);
    }
   
    protected IToggleBreakpointsTarget createToggleBreakpointsTarget() {
        if (mBreakpointTarget == null) {
            mBreakpointTarget = new SchemeToggleBreakpointsTarget(this);
        }
        return mBreakpointTarget;
    }

    //
    /// Save hooks
    //
    
    private void runSaveHooks(SchemeEditor buffer) {
    	for (Iterator hooks = sSaveHooks.iterator(); hooks.hasNext();) {
			String symbol = (String) hooks.next();
			try {
				Object hook = KawaProxy.get(symbol);
				if (hook instanceof Procedure) {
					Procedure hookProcedure = (Procedure) hook;
					hookProcedure.applyN(new Object[] { buffer });
				}
				else {
					SchemeScriptPlugin.logException("Save hook is does not evaluate to a procedure: '" + symbol + "'", null);
				}
			}
			catch (Throwable exception) {
				SchemeScriptPlugin.logException("Exception occurred during save hook", exception);
			}
		}
    }
    
    public static void addSaveHook(String hook) {
    	if (hook != null && !("".equals(hook))) {
    		if (!sSaveHooks.contains(hook))
    			sSaveHooks.add(hook);
    	}
    }
    
    public static void removeSaveHook(String hook) {
    	sSaveHooks.remove(hook);
    }
    
    public void doSaveAs() {
        super.doSaveAs();
        if (mOutlinePage != null) {
            mOutlinePage.update();
        }
    }

    protected void doSetInput(IEditorInput input) throws CoreException {
        super.doSetInput(input);
        mNavigator = null;
    }

    public void doSave(IProgressMonitor monitor) {
        super.doSave(monitor);
        runSaveHooks(this);
        if (mOutlinePage != null) {
            mOutlinePage.update();
        }
    }

	public void doRevertToSaved() {
        super.doRevertToSaved();
        if (mOutlinePage != null) {
            mOutlinePage.update();
        }
    }

    //
    //// Preference changes support
    //

    protected final boolean affectsTextPresentation(final PropertyChangeEvent event) {
        String property = event.getProperty();

        return property.startsWith(ColorPreferences.PREFIX)
               || property.startsWith(SyntaxPreferences.PREFIX)
               || property.startsWith(SchemeLexicalExtensionsPreferences.PREFIX);
    }

    protected void handlePreferenceStoreChanged(PropertyChangeEvent event) {
        String property = event.getProperty();
        if (property.equals(SchemePreferences.TAB_WIDTH)) {
            getSourceViewer().getTextWidget().setTabs(SchemePreferences.getTabWidth());
        }
        else if (affectsTextPresentation(event)) {
            SchemeTextTools textTools = SchemeScriptPlugin.getDefault().getTextTools();
            textTools.updateColors(event);

            IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
            mParenPainter.setHighlightColor(new Color(null,
                                                      PreferenceConverter.getColor(store,
                                                                                   ColorPreferences.MATCHER_COLOR)));
            mParenPainter.setParenthesisColor(new Color(null,
                                                        PreferenceConverter.getColor(store,
                                                                                     ColorPreferences.PAREN_COLOR)));
            mParenPainter.setHighlightStyle(store.getBoolean(ColorPreferences.MATCHER_BOX));
            setupBackgroundColor();
        }
        else if (property.startsWith(IndentationPreferences.PREFIX)) {
            SchemeTextTools textTools = SchemeScriptPlugin.getDefault().getTextTools();
            textTools.updateIndentationSchemes();
        }
        else if (property.equals(SchemePreferences.SEXP_EDIT)) {
            if (SchemePreferences.isStructuralEditingEnabled()) 
                addAutoEditStrategies();
            else
                removeAutoEditStrategies();
        }
        super.handlePreferenceStoreChanged(event);
    }

    public final void createPartControl(final Composite parent) {
        super.createPartControl(parent);
        mPaintManager = new PaintManager(getSourceViewer());
        startParenthesisHighlighting();

        if (SchemePreferences.isStructuralEditingEnabled())
            addAutoEditStrategies();
        
        setupBackgroundColor();
    }
    
    private void setupBackgroundColor() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        Color color = null; 
        if (!store.getBoolean(ColorPreferences.SYSTEM_BACKGROUND)) {
            color = new Color(null, PreferenceConverter.getColor(store, ColorPreferences.BACKGROUND_COLOR));
        }
        else {
            store = EditorsUI.getPreferenceStore();
            if (store.contains(AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND))
                color = new Color(null, PreferenceConverter.getColor(store, AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND));
            else
                color = new Color(null, ISchemeColorConstants.SCHEME_BACKGROUND);
        }
        getSourceViewer().getTextWidget().setBackground(color);
    }

    private void addAutoEditStrategies() {
        createEditStrategies();

        final ISourceViewer viewer = getSourceViewer();
        final ITextViewerExtension2 viewer2 = (ITextViewerExtension2) viewer;

        if (viewer2 == null) return;
        
        viewer2.prependAutoEditStrategy(mMatchinDelimiterInserter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer2.prependAutoEditStrategy(mQuoteInserter, SchemePartitionScanner.SCHEME_STRING);
        viewer2.prependAutoEditStrategy(mSexpDeleter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer2.prependAutoEditStrategy(mStringDeleter, SchemePartitionScanner.SCHEME_STRING);
        viewer2.prependAutoEditStrategy(mStringDeleter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer2.prependAutoEditStrategy(mCommentDeleter, SchemePartitionScanner.SCHEME_COMMENT);
        viewer2.prependAutoEditStrategy(mCommentDeleter, IDocument.DEFAULT_CONTENT_TYPE);
    }
    
    private void removeAutoEditStrategies() {
        createEditStrategies();
        
        ITextViewerExtension2 viewer = (ITextViewerExtension2) getSourceViewer();
        if (viewer == null) return;
        
        viewer.removeAutoEditStrategy(mMatchinDelimiterInserter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer.removeAutoEditStrategy(mQuoteInserter, SchemePartitionScanner.SCHEME_STRING);
        viewer.removeAutoEditStrategy(mSexpDeleter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer.removeAutoEditStrategy(mStringDeleter, SchemePartitionScanner.SCHEME_STRING);
        viewer.removeAutoEditStrategy(mStringDeleter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer.removeAutoEditStrategy(mCommentDeleter, SchemePartitionScanner.SCHEME_COMMENT);
        viewer.removeAutoEditStrategy(mCommentDeleter, IDocument.DEFAULT_CONTENT_TYPE);
    }

    private void createEditStrategies() {
        if (mMatchinDelimiterInserter == null || mQuoteInserter == null || mSexpDeleter == null) {
            final ISourceViewer viewer = getSourceViewer();

            mMatchinDelimiterInserter = new MatchingDelimitersInserter(viewer);
            mQuoteInserter = new DoubleQuoteInserter();
            mSexpDeleter = new SexpDeleter();
            mStringDeleter = new StringDeleter();
            mCommentDeleter = new CommentDeleter();
        }
    }

    //
    //// SymbolDictionary support
    //

    public ISymbolDictionary getSymbolDictionary() {
        return getSchemeSymbolDictionary();
    }

    public static ISymbolDictionary getSchemeSymbolDictionary() {
        if (mDictionary == null) {
            URL url = SchemeScriptPlugin.getDefault().find(new Path("conf/forms.scm"));
            mDictionary = UserDictionary.createInstance(KawaDictionary.getInstance(), "scm,ss,sch,brl,krl", url);
        }
        return mDictionary;
    }

    protected void initializeKeyBindingScopes() {
        setKeyBindingScopes(new String[]
        {
            "Scheme Editing", "Edit"
        });
    }

    //
    //// Actions, menus
    //

    protected void createActions() {
        super.createActions();
        SchemeEditorActionsFactory.createActions(this, getSourceViewer());
    }
    

    protected void editorContextMenuAboutToShow(IMenuManager menu) {
        super.editorContextMenuAboutToShow(menu);
        menu.add(new Separator("group.sexp"));
        menu.add(new Separator("group.comments"));

        MenuManager sourceMenu = new MenuManager("Source");
        menu.add(sourceMenu);

        sourceMenu.add(new Separator("group.find"));
        addAction(sourceMenu, "group.find", SchemeActionConstants.JUMP_DEF);
        addAction(sourceMenu, "group.find", "ContentAssistProposal");
        addAction(sourceMenu, "group.find", "ContentAssistTip");

        sourceMenu.add(new Separator("group.comments"));
        addAction(sourceMenu, "group.comments", SchemeActionConstants.COMMENT_HEADER);
        addAction(sourceMenu, "group.comments", SchemeActionConstants.COMMENT_CHAPTER);
        addAction(sourceMenu, "group.comments", SchemeActionConstants.COMMENT_SECTION);
        addAction(sourceMenu, "group.comments", SchemeActionConstants.COMMENT_SELECTION);

        sourceMenu.add(new Separator("group.edit"));
        addAction(sourceMenu, "group.edit", SchemeActionConstants.COMPRESS_SPACES);
        addAction(sourceMenu, "group.edit", SchemeActionConstants.SEXP_FORMAT);
        addAction(sourceMenu, "group.edit", SchemeActionConstants.SEXP_SWAP);

        MenuManager evalMenu = new MenuManager("Eval");
        menu.add(evalMenu);

        evalMenu.add(new Separator("group.eval"));
        addAction(evalMenu, "group.eval", SchemeActionConstants.EVAL_DEF);
        addAction(evalMenu, "group.eval", SchemeActionConstants.EVAL_EXPR);
        evalMenu.add(new Separator("group.load"));
        addAction(evalMenu, "group.load", SchemeActionConstants.EVAL_LOAD);
    }

    //
    //// Parenthesis highlighting
    //

    private void startParenthesisHighlighting() {
        if (mParenPainter == null) {
            ISourceViewer sourceViewer = getSourceViewer();
            IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
            mParenPainter = new SchemeParenthesisPainter(sourceViewer, this);
            mParenPainter.setHighlightStyle(store.getBoolean(ColorPreferences.MATCHER_BOX));
            mParenPainter.setHighlightColor(new Color(null,
                                                      PreferenceConverter.getColor(store,
                                                                                   ColorPreferences.MATCHER_COLOR)));
            mParenPainter.setParenthesisColor(new Color(null,
                                                        PreferenceConverter.getColor(store,
                                                                                     ColorPreferences.PAREN_COLOR)));
            mPaintManager.addPainter(mParenPainter);
        }
    }

    //
    //// Text editing helper methods
    //
    
    public final ITextViewer getTextViewer() {
    	return getSourceViewer();
    }
    

    public SexpNavigator getExplorer() {
        if (mNavigator == null) {
            mNavigator = new SexpNavigator(getDocument());
        }
        return mNavigator;
    }
    
    public IFile getFile() {
    	IEditorInput input = getEditorInput();
    	if (input instanceof FileEditorInput) {
    		return ((FileEditorInput)input).getFile();
    	}
    	else {
    		return null;
    	}
    }
    
    public String getFileName() {
    	IFile file = getFile();
    	if (file != null) {
    		return file.getRawLocation().toString();
    	}
    	else {
    		return null;
    	}
    }

    public SchemeIndentationManager getIndentationManager() {
        return ((SchemeConfiguration) getSourceViewerConfiguration()).getTextTools().getIndentationManager();
    }

    public IDocument getDocument() {
        return this.getDocumentProvider().getDocument(this.getEditorInput());
    }

    public final Region getSelection() {
        Point selection = this.getSourceViewer().getSelectedRange();
        Region r = new Region(selection.x, selection.y);
        return r;
    }

    public int getPoint() {
        return this.getSourceViewer().getTextWidget().getCaretOffset();
    }

    public void setPoint(int offset) {
        this.getSourceViewer().setSelectedRange(offset, 0);
        getSourceViewer().revealRange(offset, 0);
    }

    public int getOffset(int x, int y) {
        return getSourceViewer().getTextWidget().getOffsetAtLocation(new Point(x, y));
    }

    public void setSelection(int start, int end) {
        Assert.isTrue(start <= end);
        getSourceViewer().setSelectedRange(start, end - start);
        getSourceViewer().revealRange(start, end - start);
    }

    public int getColumn(int offset) {
        try {
            return offset - getDocument().getLineOffset(offset);
        }
        catch (BadLocationException exception) {
            return 0;
        }
    }

    public void insertText(int offset, String text) {
        replaceText(offset, 0, text);
    }

    public void replaceText(int offset, int length, String text) {
        try {
            getDocument().replace(offset, length, text);
        }
        catch (BadLocationException exception) {
        }
    }

    public void swapText(int offset1, int length1, int offset2, int length2) {
        startCompoundChange();
        try {
            String text1 = getDocument().get(offset1, length1);
            String text2 = getDocument().get(offset2, length2);
            getDocument().replace(offset2, length2, text1);
            getDocument().replace(offset1, length1, text2);
        }
        catch (BadLocationException exception) {
        }
        endCompoundChange();
    }

    public void startCompoundChange() {
        ITextViewerExtension textViewer = (ITextViewerExtension) getSourceViewer();
        textViewer.setRedraw(false);
        getSourceViewerConfiguration().getUndoManager(getSourceViewer()).beginCompoundChange();
    }

    public void endCompoundChange() {
        ITextViewerExtension textViewer = (ITextViewerExtension) getSourceViewer();
        textViewer.setRedraw(true);
        getSourceViewerConfiguration().getUndoManager(getSourceViewer()).endCompoundChange();
    }

    public void runCompoundChange(Runnable runnable) {
        try {
            startCompoundChange();
            runnable.run();
        }
        finally {
            endCompoundChange();
        }
    }

    public final String getText(int offset, int length) {
        try {
            return getDocument().get(offset, length);
        }
        catch (BadLocationException exception) {
            return "";
        }
    }

    public final char getChar(int offset) {
        try {
            return getDocument().getChar(offset);
        }
        catch (BadLocationException exception) {
            return 0;
        }
    }

    /**
     * Returns the string that precedes offset on offset's line.
     */
    public final String getIdentation(int offset) {
        int column = getColumn(offset);
        return getText(offset - column, column);
    }
}