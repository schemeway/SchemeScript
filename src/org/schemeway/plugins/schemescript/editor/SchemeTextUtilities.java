/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.views.*;

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

	public static void openOrSelectEntry(SymbolEntry[] entries, IResource localResource) {
		SymbolEntry entry = null;
		if (entries.length == 1) {
		    entry = entries[0];
		}
		else {
		    entries = SchemeTextUtilities.boostPriorities(entries, localResource);
		    DefinitionListView.showInView(entries);
		}
	
		if (entry != null && entry.getFile() == null)
		    entry = null;
	
		if (entry != null) {
		    DefinitionListView.openEditorAtLine(entry);
		}
	}

	private static SymbolEntry[] boostPriorities(SymbolEntry[] entries, final IResource localResource) {
	    List list = Arrays.asList(entries);
	
	    Collections.sort(list, new Comparator() {
	        public int compare(Object o1, Object o2) {
	            SymbolEntry e1 = (SymbolEntry) o1;
	            SymbolEntry e2 = (SymbolEntry) o2;
	            int p1 = e1.getPriority();
	            int p2 = e2.getPriority();
				if (e1.getFile() != null && e1.getFile().equals(localResource))
	                p1 += 10;
	            if (e2.getFile() != null && e2.getFile().equals(localResource))
	                p2 += 10;
	            if (p1 < p2)
	                return 1;
	            if (p1 == p2)
	                return 0;
	            else
	                return -1;
	        }
	    });
	
	    return (SymbolEntry[]) list.toArray(new SymbolEntry[list.size()]);
	}
}
