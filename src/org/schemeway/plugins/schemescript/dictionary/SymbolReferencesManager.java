/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.filebuffers.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.parser.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SymbolReferencesManager implements IResourceChangeListener {

	private SymbolReferencesTable mReferencesTable = new SymbolReferencesTable();

	public SymbolReferencesManager() {
	}

	public void resourceChanged(IResourceChangeEvent event) {
	}

	public SymbolReferencesTable getSymbolReferencesTable() {
		return mReferencesTable;
	}

	public void scanResourceForSymbols(IResource resource) {
		if (!(resource instanceof IFile)) {
			return;
		}

		IFile file = (IFile) resource;
		ITextFileBufferManager fileBufferManager = FileBuffers.getTextFileBufferManager();

		IPath path = file.getFullPath();
		try {
			fileBufferManager.connect(path, null);
			try {
				scanDocumentForSymbols(resource, fileBufferManager.getTextFileBuffer(path).getDocument());
			} finally {
				fileBufferManager.disconnect(path, null);
			}
		} catch (CoreException exception) {
			// simply ignore
		}
	}

	private void scanDocumentForSymbols(IResource resource, IDocument document) {
		mReferencesTable.removeReferences(resource);
		
		SchemeScanner scanner = new SchemeScanner();
		scanner.setRange(document, 0, document.getLength());
		
		SchemeToken token = scanner.nextToken();
		while (token != SchemeToken.EOF) {
			if (token.getType() == SchemeToken.SYMBOL) {
				int offset = scanner.getTokenOffset();
				int length = scanner.getTokenLength();
				String symbol = scanner.getText(offset, length);
				if (!".".equals(symbol)) {
					mReferencesTable.addEntry(symbol, resource, offset, length);
				}
			}
			token = scanner.nextToken();
		}
	}
}
