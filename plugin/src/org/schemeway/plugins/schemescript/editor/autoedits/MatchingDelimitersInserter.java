/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.schemeway.plugins.schemescript.parser.*;

public class MatchingDelimitersInserter implements IAutoEditStrategy {
    private final ISourceViewer viewer;

    public MatchingDelimitersInserter(ISourceViewer theViewer) {
        super();
        this.viewer = theViewer;
    }

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        try {
            if (command.text.length() != 1)
                return;

            char insertedChar = command.text.charAt(0);

            switch (insertedChar) {
                case '"':
                    if (command.offset > 0 && document.getChar(command.offset - 1) == '"') {
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
                    insertMatchingDelimiter(document, command, ")");
                    break;
                case '[':
                	insertMatchingDelimiter(document, command, "]");
                    break;
                case '{':
                	if (SchemeScannerUtilities.bracketsAreParentheses()) {
                		insertMatchingDelimiter(document, command, "}");
                		break;
                	}
                	else
                		return;
                case ']':
                	if (SchemeScannerUtilities.bracketsAreParentheses()) {
                		movePastClosingDelimiter(document, command, insertedChar, ']');
                		break;
                	}
                	else 
            			return;
                case '}':
                	if (SchemeScannerUtilities.bracketsAreParentheses()) {
                		movePastClosingDelimiter(document, command, insertedChar, '}');
                		break;
                	}
                	else
                		return;
                case ')':
                    movePastClosingDelimiter(document, command, insertedChar, ')');
                    break;
            }
        }
        catch (BadLocationException e) {
        }
    }

	private void movePastClosingDelimiter(IDocument document, DocumentCommand command, char insertedChar, char closingDelimiter)
			throws BadLocationException {
		if (command.offset < document.getLength()) {
		    char previousChar = document.getChar(command.offset);
		    int n;

		    if (previousChar == ')' || previousChar == ']' || previousChar == '}') {
		        command.length = 1;
		        command.caretOffset = command.offset + 1;
		        command.text = new String(new char[] {previousChar});
		    }
		    else if ((n = findNextRParen(insertedChar, document, command.offset)) > 0) {
		        command.length = n;
		    }
		    else
		        command.text = "";
		}
		else 
		    command.text = "";
	}

	private void insertMatchingDelimiter(IDocument document, DocumentCommand command, String closingDelimiter) throws BadLocationException {
		document.replace(command.offset + command.length, 0, closingDelimiter);
		viewer.setSelectedRange(command.offset + 1, 0);
		command.length = 0;
		command.doit = false;
	}

    private static int findNextRParen(char charToMatch, IDocument document, int offset) throws BadLocationException {
        int max = document.getLength();
        int start = offset;
        while (offset < max && Character.isWhitespace(document.getChar(offset)))
            offset++;

        if (offset == max)
            return -1;
        if (document.getChar(offset) == charToMatch)
            return offset - start + 1;

        return -1;
    }
}