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

public class BackwardSExpAction extends Action
{
    private SchemeEditor mEditor;
    private boolean mSelectExpression;
    
    public BackwardSExpAction(SchemeEditor editor, boolean selectExpression)
    {
        Assert.isNotNull(editor);
        setText("Select Backward Sexp");
        setToolTipText("Selects the previous S-expression");
        mEditor = editor;
        mSelectExpression = selectExpression;
    }
    
    public void run()
    {
        Region selection = mEditor.getSelection();
        SexpExplorer explorer = mEditor.getExplorer();
        if (explorer.backwardSexpression(selection.getOffset()))
        {
            int start = explorer.getSexpStart();
            if (mSelectExpression) 
            {
                int end = explorer.getSexpEnd(); 
                if (selection.getLength() > 0)
                {
                    end =  selection.getOffset() + selection.getLength();
                }
                mEditor.setSelection(start, end);
            }
            else
            {
                mEditor.setPoint(start);
            }
        }
    }
}
