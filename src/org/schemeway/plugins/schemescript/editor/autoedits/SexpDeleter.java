/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.parser.*;

public class SexpDeleter implements IAutoEditStrategy {

    public SexpDeleter() {
        super();
    }

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        try {
            if (command.text.length() == 0 && command.length == 1) {
                char currentChar;
                currentChar = document.getChar(command.offset);
                if (currentChar == '(' || currentChar == '[') {
                    deleteForwardSexp(document, command);
                }
                else if (currentChar == ')' || currentChar == ']') {
                    deleteBackwardSexp(document, command);
                }
            }
        }
        catch (BadLocationException e) {
        }
    }

    private void deleteForwardSexp(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        if (navigator.forwardSexpression(command.offset)) {
            int start = navigator.getSexpStart();
            int end   = navigator.getSexpEnd();
            command.length = end - start;
        }
    }

    private void deleteBackwardSexp(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        if (navigator.backwardSexpression(command.offset + 1)) {
            int start = navigator.getSexpStart();
            int end   = navigator.getSexpEnd();
            command.offset = start;
            command.length = end - start;
        }
    }
}