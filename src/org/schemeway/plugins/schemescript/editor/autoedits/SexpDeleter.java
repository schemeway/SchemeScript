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
                    SexpUtils.deleteForwardSexp(document, command);
                }
                else if (currentChar == ')' || currentChar == ']') {
                    SexpUtils.deleteBackwardSexp(document, command);
                }
            }
            else if (command.text.length() == 0 && command.length > 0) {
                if (SexpUtils.whitespacesOnly(document, command.offset, command.length))
                    return;
                SexpUtils.deleteSelection(document, command);
            }
        }
        catch (BadLocationException e) {
        }
    }

}