/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class CommentAction extends Action {
    private SchemeEditor mEditor;

    public CommentAction(SchemeEditor editor) {
        Assert.isNotNull(editor);
        setText("Toggle comment");
        setToolTipText("Comment/Uncomment the selected lines");
        mEditor = editor;
    }

    public void run() {
        try {
            final IDocument document = mEditor.getDocument();
            Region selection = mEditor.getSelection();

            final int startLine = document.getLineOfOffset(selection.getOffset());
            final int endLine = document.getLineOfOffset(selection.getOffset() + selection.getLength());
            mEditor.runCompoundChange(new Runnable() {
                public void run() {
                    processLines(document, startLine, endLine);
                }
            });
        }
        catch (BadLocationException exception) {
        }
    }

    private static void processLines(IDocument document, int startLine, int endLine) {
        String prefix = CommentPreferences.getCommentPrefix();
        int prefixLength = prefix.length();

        try {
            for (int line = startLine; line <= endLine; line++) {
                IRegion lineInfo = document.getLineInformation(line);
                int lineOffset = lineInfo.getOffset();
                int lineLength = lineInfo.getLength();

                if (lineLength >= prefixLength && document.get(lineOffset, prefixLength).equals(prefix)) {
                    document.replace(lineOffset, prefixLength, "");
                }
                else {
                    if (lineLength > 0) {
                        document.replace(lineOffset, 0, prefix);
                    }
                }
            }
        }
        catch (BadLocationException exception) {
        }
    }
}