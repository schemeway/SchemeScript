/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.interpreter.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class FastEvalAction extends Action {
    private int mIndex;

    public FastEvalAction(int index) {
        setText("Fast Eval " + index);
        setToolTipText("Evaluates a predefined expression");
        mIndex = index;
    }

    public void run() {
        String expr = FastEvalPreferences.getFastEval(mIndex);
        if (expr != null && !expr.equals("")) {
            Interpreter interp = SchemeScriptPlugin.getDefault().getInterpreter();
            interp.eval(expr);
        }
    }
}