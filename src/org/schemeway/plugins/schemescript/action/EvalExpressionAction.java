/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import java.io.*;

import org.eclipse.debug.core.model.*;
import org.eclipse.debug.ui.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.text.*;
import org.eclipse.ui.console.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class EvalExpressionAction extends Action
{
    private SchemeEditor mEditor;
    private boolean      mMoveToTop;

    public EvalExpressionAction(SchemeEditor editor, boolean topExpression)
    {
        Assert.isNotNull(editor);
        setText("Send text to interpreter");
        setToolTipText("Send text to interpreter");
        mEditor = editor;
        mMoveToTop = topExpression;
    }

    public void run()
    {
        IProcess process = DebugUITools.getCurrentProcess();
        if (process != null)
        {
            String textToEval = null;
            SexpExplorer explorer = mEditor.getExplorer();

            Region selection = mEditor.getSelection();
            if (selection.getLength() > 0)
            {
                textToEval = mEditor.getText(selection.getOffset(), selection.getLength());
            }
            if (mMoveToTop)
            {
                int start = mEditor.getPoint();
                int end = start;
                while (explorer.upSexpression(start)) {
                    start = explorer.getSexpStart();
                    end = explorer.getSexpEnd();
                }
                if (start != end && explorer.forwardSexpression(start))
                    textToEval = explorer.getText();
            }
            else
            {
                int point = mEditor.getPoint();
                if (explorer.backwardSexpression(point))
                {
                    textToEval = explorer.getText();
                }
            }
            if (textToEval != null)
            {
                evalText(process, textToEval);
            }
        }
    }

    private void evalText(IProcess process, String textToEval)
    {
        try
        {
            process.getStreamsProxy().write(textToEval + "\n");
        }
        catch (IOException exception)
        {
            MessageDialog.openError(null, "Evaluation error!", exception.getMessage());
        }
        // show the console
        IConsole console = DebugUITools.getConsole(process);
        ConsolePlugin.getDefault().getConsoleManager().showConsoleView(console);
    }
}