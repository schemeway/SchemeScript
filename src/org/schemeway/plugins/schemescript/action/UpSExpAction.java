/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class UpSExpAction extends Action {
    private SchemeEditor mEditor;
    private boolean mSelectExpression;
    private SelectionStack mStack;
    private Region mPreviousSelection;

    public UpSExpAction(SchemeEditor editor, boolean selectExpression, SelectionStack stack) {
        Assert.isNotNull(editor);
        setText("Moves to enclosing S-expression");
        setToolTipText("Moves to the enclosing S-expression");
        mEditor = editor;
        mSelectExpression = selectExpression;
        mStack = stack;
        mPreviousSelection = null;
    }

    public void run() {
        Region selection = mEditor.getSelection();

        SexpExplorer explorer = mEditor.getExplorer();
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
                mEditor.setSelection(start, end);
            }
            else {
                if (mStack != null) {
                    mStack.clear();
                }
                mEditor.setPoint(explorer.getSexpStart());
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