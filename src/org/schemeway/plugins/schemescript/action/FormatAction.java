/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.parser.*;

public class FormatAction extends Action {
    private SchemeEditor mEditor;

    public FormatAction(SchemeEditor editor) {
        Assert.isNotNull(editor);
        setText("Format");
        setToolTipText("Formats the current selection");
        mEditor = editor;
    }

    public void run() {
        final Region selection = mEditor.getSelection();
        final SexpExplorer explorer = mEditor.getExplorer();
        final IDocument document = explorer.getDocument();
        final SchemeIndentationContext context = new SchemeIndentationContext(explorer,
                                                                              mEditor.getIndentationManager(),
                                                                              0);
        final int selectionOffset = selection.getOffset();

        try {
            final int firstLine = document.getLineOfOffset(selectionOffset);
            final int lastLine = document.getLineOfOffset(selectionOffset + selection.getLength());

            mEditor.runCompoundChange(new Runnable() {
                public void run() {
                    indentLines(document, firstLine, lastLine, context);
                    if (selection.getLength() == 0) {
                        mEditor.setPoint(repositionPoint(document, selectionOffset, firstLine));
                    }
                }
            });
        }
        catch (BadLocationException exception) {
        }
    }

    public static void indentLines(IDocument document, int firstLine, int lastLine, SchemeIndentationContext context) {
        try {
            for (int lineIndex = firstLine; lineIndex <= lastLine; lineIndex++) {
                indentLine(document, lineIndex, context);
            }
        }
        catch (BadLocationException exception) {
        }
    }

    public static void indentLine(IDocument document, int lineNo, SchemeIndentationContext context)
            throws BadLocationException {
        int lineOffset = document.getLineOffset(lineNo);
        ITypedRegion partition = document.getPartition(lineOffset);
        context.setOffset(lineOffset);
        if (partition.getType() == IDocument.DEFAULT_CONTENT_TYPE) {
            int newIndentation = SchemeIndentationStrategy.findIndentation(context);
            int oldIndentation = SchemeIndentationStrategy.indentationLength(document, lineOffset);
            String indentString = SchemeIndentationStrategy.makeIndentationString(newIndentation);
            document.replace(lineOffset, oldIndentation, indentString);
        }
    }

    public static int repositionPoint(IDocument document, int initialPosition, int lineNo) {
        try {
            int lineOffset = document.getLineOffset(lineNo);
            int indentation = SchemeIndentationStrategy.indentationLength(document, lineOffset);
            if (initialPosition < lineOffset + indentation) {
                return lineOffset + indentation;
            }
        }
        catch (BadLocationException exception) {
        }
        return initialPosition;
    }
}