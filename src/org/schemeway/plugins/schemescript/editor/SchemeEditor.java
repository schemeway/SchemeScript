/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import java.net.*;

import org.eclipse.core.runtime.*;
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
import org.eclipse.ui.texteditor.*;
import org.eclipse.ui.views.contentoutline.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.action.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.autoedits.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeEditor extends TextEditor {
    private static ISymbolDictionary mDictionary;

    private SexpNavigator mNavigator;
    private PaintManager mPaintManager;
    private SchemeParenthesisPainter mParenPainter;
    private ISchemeOutlinePage mOutlinePage;

    private IAutoEditStrategy mQuoteInserter;
    private IAutoEditStrategy mMatchinDelimiterInserter;
    private IAutoEditStrategy mSexpDeleter;
    private IAutoEditStrategy mStringDeleter;

    public SchemeEditor() {
        super();
        SchemeTextTools textTools = SchemeScriptPlugin.getDefault().getTextTools();
        setSourceViewerConfiguration(new SchemeConfiguration(textTools, this));
        setPreferenceStore(SchemeScriptPlugin.getDefault().getPreferenceStore());
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
        return super.getAdapter(adapter);
    }

    protected ISchemeOutlinePage createOutlinePage() {
        return new SchemeOutlinePage(this);
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
        if (property.equals(ColorPreferences.BACKGROUND_COLOR)) {
            getSourceViewer().getTextWidget().setBackground(new Color(null, (RGB) event.getNewValue()));
        }
        else if (property.equals(SchemePreferences.TAB_WIDTH)) {
            getSourceViewer().getTextWidget().setTabs(SchemePreferences.getTabWidth());
        }
        else if (affectsTextPresentation(event)) {
            SchemeTextTools textTools = SchemeScriptPlugin.getDefault().getTextTools();
            textTools.updateColors(event);

            IPreferenceStore store = getPreferenceStore();
            mParenPainter.setHighlightColor(new Color(null,
                                                      PreferenceConverter.getColor(store,
                                                                                   ColorPreferences.MATCHER_COLOR)));
            mParenPainter.setParenthesisColor(new Color(null,
                                                        PreferenceConverter.getColor(store,
                                                                                     ColorPreferences.PAREN_COLOR)));
            mParenPainter.setHighlightStyle(store.getBoolean(ColorPreferences.MATCHER_BOX));
        }
        else if (property.startsWith(IndentationPreferences.PREFIX)) {
            SchemeTextTools textTools = SchemeScriptPlugin.getDefault().getTextTools();
            textTools.updateIndentationSchemes();
        }
        else if (property.equals(SchemePreferences.SEXP_EDIT)) {
            if (SchemePreferences.getSexpEditing()) 
                addAutoEditStrategies();
            else
                removeAutorEditStrategies();
        }
        super.handlePreferenceStoreChanged(event);
    }

    public final void createPartControl(final Composite parent) {
        super.createPartControl(parent);
        mPaintManager = new PaintManager(getSourceViewer());
        IPreferenceStore store = getPreferenceStore();
        Color color = new Color(null, PreferenceConverter.getColor(store, ColorPreferences.BACKGROUND_COLOR));
        getSourceViewer().getTextWidget().setBackground(color);
        startParenthesisHighlighting();

        if (SchemePreferences.getSexpEditing())
            addAutoEditStrategies();
    }

    private void addAutoEditStrategies() {
        createEditStrategies();

        final ISourceViewer viewer = getSourceViewer();
        final ITextViewerExtension2 viewer2 = (ITextViewerExtension2) viewer;

        viewer2.prependAutoEditStrategy(mMatchinDelimiterInserter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer2.prependAutoEditStrategy(mQuoteInserter, SchemePartitionScanner.SCHEME_STRING);
        viewer2.prependAutoEditStrategy(mSexpDeleter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer2.prependAutoEditStrategy(mStringDeleter, SchemePartitionScanner.SCHEME_STRING);
        viewer2.prependAutoEditStrategy(mStringDeleter, IDocument.DEFAULT_CONTENT_TYPE);
    }
    
    private void removeAutorEditStrategies() {
        createEditStrategies();
        
        ITextViewerExtension2 viewer = (ITextViewerExtension2) getSourceViewer();
        viewer.removeAutoEditStrategy(mMatchinDelimiterInserter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer.removeAutoEditStrategy(mQuoteInserter, SchemePartitionScanner.SCHEME_STRING);
        viewer.removeAutoEditStrategy(mSexpDeleter, IDocument.DEFAULT_CONTENT_TYPE);
        viewer.removeAutoEditStrategy(mStringDeleter, SchemePartitionScanner.SCHEME_STRING);
        viewer.removeAutoEditStrategy(mStringDeleter, IDocument.DEFAULT_CONTENT_TYPE);
    }

    private void createEditStrategies() {
        if (mMatchinDelimiterInserter == null || mQuoteInserter == null || mSexpDeleter == null) {
            final ISourceViewer viewer = getSourceViewer();

            mMatchinDelimiterInserter = new MatchingDelimitersInserter(viewer);
            mQuoteInserter = new DoubleQuoteInserter();
            mSexpDeleter = new SexpDeleter();
            mStringDeleter = new StringDeleter();
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
            mDictionary = UserDictionary.createInstance(KawaDictionary.getInstance(), "scm", url);
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

        IAction action = new ForwardSExpAction(this, false);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_FORWARD);
        this.setAction(SchemeActionConstants.SEXP_FORWARD, action);

        action = new ForwardSExpAction(this, true);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SELECT_FORWARD);
        this.setAction(SchemeActionConstants.SEXP_SELECT_FORWARD, action);

        action = new BackwardSExpAction(this, false);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_BACKWARD);
        this.setAction(SchemeActionConstants.SEXP_BACKWARD, action);

        action = new BackwardSExpAction(this, true);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SELECT_BACKWARD);
        this.setAction(SchemeActionConstants.SEXP_SELECT_BACKWARD, action);

        action = new UpSExpAction(this, false, null);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_UP);
        this.setAction(SchemeActionConstants.SEXP_UP, action);

        SelectionStack stack = new SelectionStack();

        action = new UpSExpAction(this, true, stack);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SELECT_UP);
        this.setAction(SchemeActionConstants.SEXP_SELECT_UP, action);

        action = new RestoreSelectionAction(this, stack);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_RESTORE_SELECTION);
        this.setAction(SchemeActionConstants.SEXP_RESTORE_SELECTION, action);

        action = new DownSExpAction(this);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_DOWN);
        this.setAction(SchemeActionConstants.SEXP_DOWN, action);

        action = new SwapSexpAction(this);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SWAP);
        this.setAction(SchemeActionConstants.SEXP_SWAP, action);

        action = new FormatAction(this);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_FORMAT);
        this.setAction(SchemeActionConstants.SEXP_FORMAT, action);

        action = SectioningCommentAction.createChapterCommentAction(this);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_CHAPTER);
        this.setAction(SchemeActionConstants.COMMENT_CHAPTER, action);

        action = SectioningCommentAction.createSectionCommentAction(this);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_SECTION);
        this.setAction(SchemeActionConstants.COMMENT_SECTION, action);

        action = new CommentAction(this);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_SELECTION);
        this.setAction(SchemeActionConstants.COMMENT_SELECTION, action);

        action = new FileHeaderCommentAction(this);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_HEADER);
        this.setAction(SchemeActionConstants.COMMENT_HEADER, action);

        action = new EvalExpressionAction(this, false);
        action.setActionDefinitionId(SchemeActionConstants.EVAL_EXPR);
        this.setAction(SchemeActionConstants.EVAL_EXPR, action);

        action = new EvalExpressionAction(this, true);
        action.setActionDefinitionId(SchemeActionConstants.EVAL_DEF);
        this.setAction(SchemeActionConstants.EVAL_DEF, action);

        action = new LoadFileAction(this);
        action.setActionDefinitionId(SchemeActionConstants.EVAL_LOAD);
        this.setAction(SchemeActionConstants.EVAL_LOAD, action);

        action = new CompressSpacesAction(this);
        action.setActionDefinitionId(SchemeActionConstants.COMPRESS_SPACES);
        this.setAction(SchemeActionConstants.COMPRESS_SPACES, action);
        
        action = new MoveRightParenForwardOneExpression(this);
        action.setActionDefinitionId(SchemeActionConstants.MOVE_RPAREN_FORWARD);
        this.setAction(SchemeActionConstants.MOVE_RPAREN_FORWARD, action);

        action = new MoveLeftParenBackwardOneExpression(this);
        action.setActionDefinitionId(SchemeActionConstants.MOVE_LPAREN_BACKWARD);
        this.setAction(SchemeActionConstants.MOVE_LPAREN_BACKWARD, action);

        MouseCopyAction mouseAction = new MouseCopyAction(this, getSourceViewer().getTextWidget(), false);
        mouseAction.setActionDefinitionId(SchemeActionConstants.SEXP_MOUSECOPY);
        this.setAction(SchemeActionConstants.SEXP_MOUSECOPY, mouseAction);

        mouseAction = new MouseCopyAction(this, getSourceViewer().getTextWidget(), true);
        mouseAction.setActionDefinitionId(SchemeActionConstants.SEXP_EXTENDED_MOUSECOPY);
        this.setAction(SchemeActionConstants.SEXP_EXTENDED_MOUSECOPY, mouseAction);

        action = new JumpToDefinitionAction(this);
        action.setActionDefinitionId(SchemeActionConstants.JUMP_DEF);
        this.setAction(SchemeActionConstants.JUMP_DEF, action);

        action = new TextOperationAction(SchemeScriptPlugin.getDefault().getResourceBundle(),
                                         "ContentAssistProposal.", this, ISourceViewer.CONTENTASSIST_PROPOSALS); //$NON-NLS-1$
        action.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        setAction("ContentAssistProposal", action);

        action = new TextOperationAction(SchemeScriptPlugin.getDefault().getResourceBundle(),
                                         "ContentAssistTip.", this, ISourceViewer.CONTENTASSIST_CONTEXT_INFORMATION); //$NON-NLS-1$
        action.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
        setAction("ContentAssistTip", action);
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
            IPreferenceStore store = getPreferenceStore();
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

    public SexpNavigator getExplorer() {
        if (mNavigator == null) {
            mNavigator = new SexpNavigator(getDocument());
        }
        return mNavigator;
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