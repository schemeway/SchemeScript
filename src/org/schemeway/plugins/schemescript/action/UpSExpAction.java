/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class UpSExpAction extends SchemeAction {
    private boolean mSelectExpression;
    private SelectionStack mStack;
    private Region mPreviousSelection;

    public UpSExpAction(SchemeEditor editor, boolean selectExpression, SelectionStack stack) {
        super(editor);
        setText("Moves to enclosing S-expression");
        setToolTipText("Moves to the enclosing S-expression");
        mSelectExpression = selectExpression;
        mStack = stack;
        mPreviousSelection = null;
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;
        
        Region selection = editor.getSelection();
        SexpNavigator explorer = editor.getExplorer();
        if (explorer.upSexpression(selection.getOffset())) {
            if (mSelectExpression && explorer.forwardSexpression(explorer.getSexpStart())) {
                int start = explorer.getSexpStart();
                int end   = explorer.getSexpEnd();
                if (mStack != null) {
                    if (mPreviousSelection != null && !encloses(mPreviousSelection, selection))
                        mPreviousSelection = null;
                    mStack.push(selection, mPreviousSelection);
                    mPreviousSelection = selection;
                }
                editor.setSelection(start, end);
            }
            else {
                if (mStack != null) {
                    mStack.clear();
                }
                editor.setPoint(explorer.getSexpStart());
            }
        }
    }
    
    private boolean encloses(Region innerRegion, Region outerRegion) {
        int innerStart = innerRegion.getOffset();
        int innerEnd   = innerRegion.getLength() + innerStart;
        int outerStart = outerRegion.getOffset();
        int outerEnd   = outerRegion.getLength() + outerStart;
        
        return (innerStart >= outerStart && innerStart <= outerEnd && innerEnd < outerEnd);
    }
}