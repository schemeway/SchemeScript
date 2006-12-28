/*
 * Copyright (c) 2005 SchemeWay.com
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.ui.texteditor.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.action.*;

/**
 * @author SchemeWay.com
 */
public final class SchemeEditorActionsFactory {
    private SchemeEditorActionsFactory() {
        // must not be instantiated
    }
    
    public static void createActions(SchemeEditor editor, ISourceViewer sourceViewer) {
        IAction action = new ForwardSExpAction(editor, false);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_FORWARD);
        editor.setAction(SchemeActionConstants.SEXP_FORWARD, action);

        action = new ForwardSExpAction(editor, true);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SELECT_FORWARD);
        editor.setAction(SchemeActionConstants.SEXP_SELECT_FORWARD, action);

        action = new BackwardSExpAction(editor, false);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_BACKWARD);
        editor.setAction(SchemeActionConstants.SEXP_BACKWARD, action);

        action = new BackwardSExpAction(editor, true);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SELECT_BACKWARD);
        editor.setAction(SchemeActionConstants.SEXP_SELECT_BACKWARD, action);

        action = new UpSExpAction(editor, false, null);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_UP);
        editor.setAction(SchemeActionConstants.SEXP_UP, action);

        SelectionStack stack = new SelectionStack();

        action = new UpSExpAction(editor, true, stack);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SELECT_UP);
        editor.setAction(SchemeActionConstants.SEXP_SELECT_UP, action);

        action = new RestoreSelectionAction(editor, stack);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_RESTORE_SELECTION);
        editor.setAction(SchemeActionConstants.SEXP_RESTORE_SELECTION, action);

        action = new DownSExpAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_DOWN);
        editor.setAction(SchemeActionConstants.SEXP_DOWN, action);

        action = new SwapSexpAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_SWAP);
        editor.setAction(SchemeActionConstants.SEXP_SWAP, action);

        action = new FormatAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_FORMAT);
        editor.setAction(SchemeActionConstants.SEXP_FORMAT, action);
        
        action = new ExpandSelectionToSexpressions(editor);
        action.setActionDefinitionId(SchemeActionConstants.SEXP_EXPAND_SELECTION);
        editor.setAction(SchemeActionConstants.SEXP_EXPAND_SELECTION, action);

        action = SectioningCommentAction.createChapterCommentAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_CHAPTER);
        editor.setAction(SchemeActionConstants.COMMENT_CHAPTER, action);

        action = SectioningCommentAction.createSectionCommentAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_SECTION);
        editor.setAction(SchemeActionConstants.COMMENT_SECTION, action);

        action = new CommentAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_SELECTION);
        editor.setAction(SchemeActionConstants.COMMENT_SELECTION, action);

        action = new FileHeaderCommentAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.COMMENT_HEADER);
        editor.setAction(SchemeActionConstants.COMMENT_HEADER, action);

        action = new EvalExpressionAction(editor, false);
        action.setActionDefinitionId(SchemeActionConstants.EVAL_EXPR);
        editor.setAction(SchemeActionConstants.EVAL_EXPR, action);

        action = new EvalExpressionAction(editor, true);
        action.setActionDefinitionId(SchemeActionConstants.EVAL_DEF);
        editor.setAction(SchemeActionConstants.EVAL_DEF, action);

        action = new LoadFileAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.EVAL_LOAD);
        editor.setAction(SchemeActionConstants.EVAL_LOAD, action);

        action = new CompressSpacesAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.COMPRESS_SPACES);
        editor.setAction(SchemeActionConstants.COMPRESS_SPACES, action);
        
        action = new MoveRightParenForwardOneExpression(editor);
        action.setActionDefinitionId(SchemeActionConstants.MOVE_RPAREN_FORWARD);
        editor.setAction(SchemeActionConstants.MOVE_RPAREN_FORWARD, action);

        action = new MoveLeftParenBackwardOneExpression(editor);
        action.setActionDefinitionId(SchemeActionConstants.MOVE_LPAREN_BACKWARD);
        editor.setAction(SchemeActionConstants.MOVE_LPAREN_BACKWARD, action);

        MouseCopyAction mouseAction = new MouseCopyAction(editor, sourceViewer.getTextWidget(), false);
        mouseAction.setActionDefinitionId(SchemeActionConstants.SEXP_MOUSECOPY);
        editor.setAction(SchemeActionConstants.SEXP_MOUSECOPY, mouseAction);

        mouseAction = new MouseCopyAction(editor, sourceViewer.getTextWidget(), true);
        mouseAction.setActionDefinitionId(SchemeActionConstants.SEXP_EXTENDED_MOUSECOPY);
        editor.setAction(SchemeActionConstants.SEXP_EXTENDED_MOUSECOPY, mouseAction);

        action = new JumpToDefinitionAction(editor);
        action.setActionDefinitionId(SchemeActionConstants.JUMP_DEF);
        editor.setAction(SchemeActionConstants.JUMP_DEF, action);
        
        addSchemeAction(editor, "sexp.kill-next", "kill-next-sexp");
        addSchemeAction(editor, "sexp.kill-previous", "kill-previous-sexp");
        addSchemeAction(editor, "sexp.raise", "raise-sexp");
        addSchemeAction(editor, "sexp.split", "split-sexp");
        addSchemeAction(editor, "sexp.join", "join-sexp");
        addSchemeAction(editor, "sexp.wrap", "wrap-sexp");
        addSchemeAction(editor, "sexp.splice", "splice-sexp");
        addSchemeAction(editor, "sexp.slurp-forward", "forward-slurp-sexp");
        addSchemeAction(editor, "sexp.slurp-backward", "backward-slurp-sexp");
        addSchemeAction(editor, "comment.multiline", "comment-selection");
        addSchemeAction(editor, "code.expandPackage", "expand-package");
        addSchemeAction(editor, "code.expandNamespace", "expand-namespace");
        addSchemeAction(editor, "code.expandTypename", "expand-typename");
        addSchemeAction(editor, "code.addRequire", "add-require-clause");
        
        addSchemeAction(editor, "sexp.rename-symbol", "rename-symbol");
        addSchemeAction(editor, "sexp.rename-symbol-locally", "rename-symbol-locally");
        addSchemeAction(editor, "sexp.extract-variable", "extract-variable");
        addSchemeAction(editor, "sexp.create-function", "create-function-from-expression");
        
        action = new TextOperationAction(SchemeScriptPlugin.getDefault().getResourceBundle(),
                                         "ContentAssistProposal.", editor, ISourceViewer.CONTENTASSIST_PROPOSALS); //$NON-NLS-1$
        action.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
        editor.setAction("ContentAssistProposal", action);

        action = new TextOperationAction(SchemeScriptPlugin.getDefault().getResourceBundle(),
                                         "ContentAssistTip.", editor, ISourceViewer.CONTENTASSIST_CONTEXT_INFORMATION); //$NON-NLS-1$
        action.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
        editor.setAction("ContentAssistTip", action);
    }
    
    private static void addSchemeAction(SchemeEditor editor, String idSuffix, String procedureName) {
    	IAction action = new SchemeProcedureAction(procedureName);
    	String id = SchemeScriptPlugin.PLUGIN_NS + "." + idSuffix;
    	action.setActionDefinitionId(id);
    	editor.setAction(id, action);
    }
}
