/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.parser.*;

/**
 * @author Nu Echo Inc.
 */
public final class SchemeTextUtilities {

	// prevent instantiation
	private SchemeTextUtilities() {
	}

	public static String findSymbolBeforePoint(ITextViewer viewer, int offset) throws BadLocationException {
		IDocument document = viewer.getDocument();
		int start = offset;
		while (start > 0 && SchemeScannerUtilities.isIdentifierPartChar(document.getChar(start - 1)))
			start--;
		if (start == offset)
			return null;
		else
			return document.get(start, offset - start);
	}

	public static String findSymbolAroundPoint(IDocument document, int offset) throws BadLocationException {
		IRegion region = findSymbolRegionAroundPoint(document, offset);
		if (region == null)
			return null;
		return document.get(region.getOffset(), region.getLength());
	}

	public static IRegion findSymbolRegionAroundPoint(IDocument document, int offset) throws BadLocationException {
		int length = document.getLength();
		while (offset > 0 && Character.isWhitespace(document.getChar(offset - 1)))
			offset--;
		int start = offset;
		while (start > 0 && SchemeScannerUtilities.isIdentifierPartChar(document.getChar(start - 1)))
			start--;
		while (offset < length && SchemeScannerUtilities.isIdentifierPartChar(document.getChar(offset)))
			offset++;
		if (start == offset)
			return null;

		return new Region(start, offset - start);
	}

	public static ITypedRegion getPartition(IDocument document, int offset) {
		try {
			if (document instanceof IDocumentExtension3) {
				IDocumentExtension3 documentExtension3 = (IDocumentExtension3) document;
				return documentExtension3.getPartition(SchemeDocumentSetupParticipant.SCHEME_PARTITIONING, offset,
						false);
			}
			else {
				return document.getPartition(offset);
			}
		}
		catch (BadLocationException e) {
			return null;
		}
		catch (BadPartitioningException e) {
			return null;
		}
	}
}
