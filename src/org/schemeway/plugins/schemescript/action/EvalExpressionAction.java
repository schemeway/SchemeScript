/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.interpreter.*;
import org.schemeway.plugins.schemescript.parser.*;

public class EvalExpressionAction extends SchemeAction {
    private boolean mMoveToTop;

    public EvalExpressionAction(SchemeEditor editor, boolean topExpression) {
        super(editor);
        
        if (topExpression)
            setText("Eval top expression");
        else
            setText("Eval previous expression");
        setToolTipText("Send text to interpreter");
        mMoveToTop = topExpression;
    }

    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null)
            return;
        
        String textToEval = null;
        SexpNavigator explorer = editor.getExplorer();

        Region selection = editor.getSelection();
        if (selection.getLength() > 0) {
            textToEval = editor.getText(selection.getOffset(), selection.getLength());
        }
        else if (mMoveToTop) {
            int start = editor.getPoint();
            int end = start;
            while (explorer.upSexpression(start)) {
                start = explorer.getSexpStart();
                end = explorer.getSexpEnd();
            }
            if (start != end && explorer.forwardSexpression(start))
                textToEval = explorer.getText();
        }
        else {
            int point = editor.getPoint();
            if (explorer.backwardSexpression(point)) {
                textToEval = explorer.getText();
            }
        }
        if (textToEval != null) {
            evalText(textToEval);
        }
    }

    private void evalText(String textToEval) {
        Interpreter interp = SchemeScriptPlugin.getDefault().getInterpreter();
        interp.eval(textToEval);
    }
}