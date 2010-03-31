/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.parser.*;

public class FormatAction extends SchemeAction {

    /**
     * 
     */
    private static final int DEFAULT_SIMPLE_COMMENT_COLUMN = 40;

    public FormatAction(SchemeEditor editor) {
        super(editor);
        setText("Format");
        setToolTipText("Formats the current selection");
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null)
            return;

        final Region selection = editor.getSelection();
        final SexpNavigator explorer = editor.getExplorer();
        final IDocument document = explorer.getDocument();
        final SchemeIndentationContext context = new SchemeIndentationContext(explorer, editor.getIndentationManager(),
                0);
        final int selectionOffset = selection.getOffset();

        try {
            final int firstLine = document.getLineOfOffset(selectionOffset);
            final int lastLine = document.getLineOfOffset(selectionOffset + selection.getLength());

            editor.runCompoundChange(new Runnable() {
                public void run() {
                    indentLines(document,
                                firstLine,
                                lastLine,
                                context,
                                editor,
                                selection.getLength() == 0 ? selectionOffset : -1);
                }
            });
        } catch (BadLocationException exception) {
        }
    }

    public static void indentLines(IDocument document,
                                   int firstLine,
                                   int lastLine,
                                   SchemeIndentationContext context,
                                   SchemeEditor editor,
                                   int pointOffset) {
        try {
            for (int lineIndex = firstLine; lineIndex <= lastLine; lineIndex++) {
                indentLine(document, lineIndex, context, editor, pointOffset);
            }
        } catch (BadLocationException exception) {
        }
    }

    public static void indentLine(IDocument document,
                                  int lineNo,
                                  SchemeIndentationContext context,
                                  SchemeEditor editor,
                                  int pointOffset) throws BadLocationException {
        IRegion lineInfo = document.getLineInformation(lineNo);
        int lineOffset = lineInfo.getOffset();
        int lineEnd = lineInfo.getLength() + lineOffset;

        ITypedRegion partition = SchemeTextUtilities.getPartition(document, lineOffset);
        context.setOffset(lineOffset);

        // re-indent line
        if (partition.getType() == IDocument.DEFAULT_CONTENT_TYPE
                || partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
            int oldIndentation = SchemeIndentationStrategy.indentationLength(document, lineOffset);
            int newIndentation = oldIndentation;

            if (lineOffset + oldIndentation < lineEnd) {
                ITypedRegion firstTokenPartition = SchemeTextUtilities.getPartition(document, lineOffset
                        + oldIndentation);
                if (firstTokenPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                    if (firstTokenPartition.getLength() >= 3
                            && document.get(firstTokenPartition.getOffset(), 3).equals(";;;")) {
                        newIndentation = 0;
                    } else if (firstTokenPartition.getLength() >= 2
                            && document.get(firstTokenPartition.getOffset(), 2).equals(";;")) {
                        newIndentation = SchemeIndentationStrategy.findIndentation(context);
                    } else if (firstTokenPartition.getLength() >= 2
                            && document.get(firstTokenPartition.getOffset(), 2).equals("#|")) {
                        return;
                    } else {
                        newIndentation = DEFAULT_SIMPLE_COMMENT_COLUMN;
                        if (lineNo > 0) {
                            IRegion previousLineInfo = document.getLineInformation(lineNo - 1);
                            ITypedRegion previousLineLastPartition = SchemeTextUtilities
                                    .getPartition(document, previousLineInfo.getOffset() + previousLineInfo.getLength()
                                            - 1);
                            if (previousLineLastPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT
                                    && document.getChar(previousLineLastPartition.getOffset()) == ';') {
                                newIndentation = previousLineLastPartition.getOffset() - previousLineInfo.getOffset();
                            }
                        }
                    }
                } else {
                    newIndentation = SchemeIndentationStrategy.findIndentation(context);
                }
            }

            String indentString = SchemeIndentationStrategy.makeIndentationString(newIndentation);
            if (indentString.length() != oldIndentation) {
                document.replace(lineOffset, oldIndentation, indentString);
                lineEnd += (indentString.length() - oldIndentation);
            }

            if (lineOffset <= pointOffset && pointOffset <= lineEnd) {
                editor.setPoint(pointOffset + (newIndentation - oldIndentation));
            }
        }
        // remove extra whitespace at end of line
        if (lineEnd > lineOffset) {
            removeExtraWhitespace(document, lineEnd - 1);
        }
    }

    private static void removeExtraWhitespace(IDocument document, int lineLastChar) throws BadLocationException {
        ITypedRegion partition;

        int index = lineLastChar;
        partition = SchemeTextUtilities.getPartition(document, index);
        if (partition.getType() == IDocument.DEFAULT_CONTENT_TYPE) {
            char c = document.getChar(index);
            while (Character.isWhitespace(c) && !(c == '\n' || c == '\r')) {
                index--;
                c = document.getChar(index);
            }
        }
        if (index != lineLastChar) {
            document.replace(index + 1, (lineLastChar - index), "");
        }
    }

    public static int repositionPoint(IDocument document, int initialPosition, int lineNo) {
        try {
            int lineOffset = document.getLineOffset(lineNo);
            int indentation = SchemeIndentationStrategy.indentationLength(document, lineOffset);
            if (initialPosition < lineOffset + indentation) {
                return lineOffset + indentation;
            }
        } catch (BadLocationException exception) {
        }
        return initialPosition;
    }
}