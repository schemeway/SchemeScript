/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;


final class MatchingDelimitersInserter implements IAutoEditStrategy {
    private final ISourceViewer viewer;

    public MatchingDelimitersInserter(ISourceViewer viewer) {
        super();
        this.viewer = viewer;
    }

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        try {
            if (command.text.length() != 1)
                return;
            
            char insertedChar = command.text.charAt(0);
            
            switch (insertedChar) {
                case '"':
                    if (document.getChar(command.offset - 1) == '"') {
                        command.doit = false;
                    }
                    if (command.offset >= 2 
                            && document.getChar(command.offset - 2) == '#'
                            && document.getChar(command.offset - 1) == '\\') {
                        return;
                    }
                    else {
                        document.replace(command.offset, 0, "\"");
                        viewer.setSelectedRange(command.offset + 1, 0);
                        command.doit = false;
                    }
                    break;
                case '(':
                    document.replace(command.offset + command.length, 0, ")");
                    viewer.setSelectedRange(command.offset + 1, 0);
                    command.length = 0;
                    command.doit = false;
                    break;
                case '[':
                    document.replace(command.offset, 0, "]");
                    viewer.setSelectedRange(command.offset + 1, 0);
                    command.doit = false;
                    break;
                case ']':
                case ')':
                    char previousChar = document.getChar(command.offset) ;
                    if (previousChar == ']' || previousChar == ')') { 
                        command.length = 1;
                        command.caretOffset = command.offset + 1;
                    }
                    break;
            }
        }
        catch (BadLocationException e) {
        }
    }
}