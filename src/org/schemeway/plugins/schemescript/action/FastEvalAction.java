/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.interpreter.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class FastEvalAction extends Action implements IWorkbenchWindowActionDelegate {
    private int mIndex;

    public static class FastEvalAction0 extends FastEvalAction {
        public FastEvalAction0() { super(0); }
    }
    public static class FastEvalAction1 extends FastEvalAction {
        public FastEvalAction1() { super(1); }
    }
    public static class FastEvalAction2 extends FastEvalAction {
        public FastEvalAction2() { super(2); }
    }
    public static class FastEvalAction3 extends FastEvalAction {
        public FastEvalAction3() { super(3); }
    }
    public static class FastEvalAction4 extends FastEvalAction {
        public FastEvalAction4() { super(4); }
    }
    public static class FastEvalAction5 extends FastEvalAction {
        public FastEvalAction5() { super(5); }
    }
    public static class FastEvalAction6 extends FastEvalAction {
        public FastEvalAction6() { super(6); }
    }
    public static class FastEvalAction7 extends FastEvalAction {
        public FastEvalAction7() { super(7); }
    }
    public static class FastEvalAction8 extends FastEvalAction {
        public FastEvalAction8() { super(8); }
    }
    public static class FastEvalAction9 extends FastEvalAction {
        public FastEvalAction9() { super(9); }
    }
    
    
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
    
    
    public void dispose() {
    }

    public void init(IWorkbenchWindow window) {
    }

    public void run(IAction action) {
        run();
    }
    
    public void selectionChanged(IAction action, ISelection selection) {
    }
}