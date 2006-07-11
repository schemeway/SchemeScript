/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class CommentAction extends SchemeAction {

	public CommentAction(SchemeEditor editor) {
		super(editor);
		setText("Toggle comment");
		setToolTipText("Comment/Uncomment the selected lines");
	}

	public void run() {
		SchemeEditor editor = getSchemeEditor();
		if (editor == null)
			return;

		try {
			final IDocument document = editor.getDocument();
			final Region selection = editor.getSelection();

			final int startLine = document.getLineOfOffset(selection.getOffset());
			final int endLine = document.getLineOfOffset(selection.getOffset() + selection.getLength());
			if (!(document.getPartition(selection.getOffset()).getType() == SchemePartitionScanner.SCHEME_COMMENT)
					|| (selection.getLength() != 0)) {
				editor.runCompoundChange(new Runnable() {
					public void run() {
						processLines(document, startLine, endLine);
					}
				});
			}
			else {
				editor.runCompoundChange(new Runnable() {
					public void run() {
						removeCommentFromPartition(document, selection.getOffset());
					}
				});
			}
		}
		catch (BadLocationException exception) {
		}
	}

	private void removeCommentFromPartition(IDocument document, int offset) {
		try {
			ITypedRegion partition = document.getPartition(offset);
			
			if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
				int partitionOffset = partition.getOffset();
				if (document.getChar(partitionOffset) == ';') {
					int commentPrefixEnd = partitionOffset;
					while (document.getChar(commentPrefixEnd) == ';') 
						commentPrefixEnd++;
					document.replace(partitionOffset, commentPrefixEnd - partitionOffset, "");
				}
				else if (document.get(partitionOffset, 2).equals("#|")) {
					document.replace(partitionOffset + partition.getLength() - 2, 2, "");
					document.replace(partitionOffset, 2, "");
				}
				else if (document.get(partitionOffset, 2).equals("#;")) {
					document.replace(partitionOffset, 2, "");
				}
			}
		}
		catch (BadLocationException e) {
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