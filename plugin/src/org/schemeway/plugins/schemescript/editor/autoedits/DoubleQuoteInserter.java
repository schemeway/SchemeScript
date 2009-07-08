/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;

public class DoubleQuoteInserter implements IAutoEditStrategy {

	public DoubleQuoteInserter() {
	}

	public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
		try {
			if (command.text.equals("\\")) {
				if (document.getChar(command.offset) == '"') {
					command.text = "\\\\";
					command.doit = true;
					command.length = 0;
					command.shiftsCaret = true;
					command.caretOffset = command.offset;
				}
			} 
			else if (command.text.equals("\"")) {
				if (document.getChar(command.offset) == '"') {
					command.doit = true;
					command.text = "";
					command.length = 0;
					command.shiftsCaret = true;
					command.caretOffset = command.offset + 1;
				}
				else {
					command.text = "\\\"";
					command.doit = true;
					command.length = 0;
					command.shiftsCaret = true;
					command.caretOffset = command.offset;
				}
			}
		} catch (BadLocationException exception) {
		}
	}
}