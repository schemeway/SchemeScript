/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public final class SexpUtils {

    private SexpUtils() {
        // should not be instantiated
    }

    public static boolean whitespacesOnly(IDocument document, int offset, int length) throws BadLocationException {
        int end = offset + length;
        for (int i = offset; i < end; i++) {
            if (!Character.isWhitespace(document.getChar(i)))
                return false;
        }
        return true;
    }

    public static void insertText(IDocument document, DocumentCommand command) throws BadLocationException {
        if (!checkCompleteSExpressions(command.text)) {
            command.length = 0;
            command.text = "";
            command.doit = false;
            MessageDialog.openError(null,
                                    "Structural editing error",
                                    "Trying to insert text with unbalanced delimiters");
        }
    }

    public static void deleteSelection(IDocument document, DocumentCommand command) throws BadLocationException {
        if (!checkCompleteSExpressions(document, command.offset, command.length)) {
            command.length = 0;
            command.text = "";
            command.doit = false;
            MessageDialog.openError(null,
                                    "Structural editing error",
                                    "Trying to delete text with unbalanced delimiters");
        }
    }

    public static void deleteForwardSexp(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        if (navigator.forwardSexpression(command.offset)) {
            int start = navigator.getSexpStart();
            int end = navigator.getSexpEnd();
            command.length = end - start;
        }
    }

    public static IRegion findEnclosingSexpressions(IDocument document, int start, int end) {
        try {
            SexpNavigator navigator = new SexpNavigator(document);

            ITypedRegion partition = document.getPartition(start);
            while (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                start = partition.getOffset() + partition.getLength();
                partition = document.getPartition(start);
            }

            partition = document.getPartition(end);
            while (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                end = partition.getOffset() - 1;
                partition = document.getPartition(end);
            }
            if (start >= end)
                return null;

            int index = end;
            while (navigator.upSexpression(index) && navigator.getSexpStart() >= start) {
                if (!navigator.forwardSexpression(navigator.getSexpStart())) {
                    return null;
                }
                index = navigator.getSexpEnd();
            }
            if (end == index && navigator.forwardSexpression(end) && navigator.getSexpStart() < end) {
                end = index = navigator.getSexpEnd();
            }
            end = index;

            while (index > start) {
                if (navigator.backwardSexpression(index) && navigator.getSexpEnd() > start)
                    index = navigator.getSexpStart();
                else
                    break;
            }
            start = index;

            return new Region(start, end - start);
        }
        catch (BadLocationException e) {
            return null;
        }
    }

    public static void deleteBackwardSexp(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        if (navigator.backwardSexpression(command.offset + 1)) {
            int start = navigator.getSexpStart();
            int end = navigator.getSexpEnd();
            command.offset = start;
            command.length = end - start;
        }
    }

    public static boolean checkCompleteSExpressions(IDocument document, int offset, int length) {
        try {
            int startOffset = offset;
            int endOffset = offset + length;
            ITypedRegion partition;

            // if the start of the region is inside a comment, go to the start
            // of the comment partition
            partition = document.getPartition(startOffset);
            if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT && partition.getOffset() < startOffset) {
                startOffset = partition.getOffset();
            }
            else if (SchemePartitionScanner.isStringPartition(partition.getType())
                     && startOffset != partition.getOffset()) {
                return false;
            }

            // if the end of the region is inside a comment, move the end just
            // before the comment partition
            partition = document.getPartition(endOffset);
            if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT && endOffset > partition.getOffset()) {
                endOffset = partition.getOffset();
            }
            else if (SchemePartitionScanner.isStringPartition(partition.getType()) && endOffset != partition.getOffset()) {
                return false;
            }

            SchemeScanner scanner = new SchemeScanner();
            scanner.setRange(document, startOffset, endOffset - startOffset);
            int level = 0;
            SchemeToken token = scanner.nextToken();
            while (token != SchemeToken.EOF) {
                switch (token.getType()) {
                    case SchemeToken.LPAREN:
                    {
                        level++;
                        break;
                    }
                    case SchemeToken.RPAREN:
                    {
                        level--;
                        if (level < 0)
                            return false;
                        break;
                    }
                    case SchemeToken.ERROR:
                    {
                        int tokenOffset = token.getOffset();
                        if (document.getChar(tokenOffset) == '"'
                            || (document.getChar(tokenOffset) == '#' && document.getChar(tokenOffset + 1) == '|'))
                            return false;
                    }
                }
                token = scanner.nextToken();
            }
            return (level == 0);
        }
        catch (BadLocationException e) {
        }

        return false;
    }

    public static boolean checkCompleteSExpressions(String text) {
        return checkCompleteSExpressions(new Document(text), 0, text.length());
    }
}