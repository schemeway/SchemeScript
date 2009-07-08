/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.runtime.*;
import org.eclipse.search.ui.*;
import org.eclipse.search.ui.text.*;
import org.schemeway.plugins.schemescript.*;

class SymbolSearchQuery implements ISearchQuery {
	private String mSearchSymbol;
	private SymbolSearchResult mSearchResult;

	public SymbolSearchQuery(String searchSymbol) {
		mSearchSymbol = searchSymbol;
		mSearchResult = new SymbolSearchResult(this);
	}

	public boolean canRunInBackground() {
		return true;
	}

	public boolean canRerun() {
		return true;
	}

	public String getLabel() {
		return "Search for " + mSearchSymbol;
	}

	public ISearchResult getSearchResult() {
		return mSearchResult;
	}

	public IStatus run(IProgressMonitor monitor) throws OperationCanceledException {
		SymbolReferencesManager referencesManager = SchemeScriptPlugin.getReferencesManager();
		SymbolReferencesTable referencesTable = referencesManager.getSymbolReferencesTable();

		Reference[] references = referencesTable.getReferences(mSearchSymbol);	
		Match[] matches = new Match[references.length];
		for (int referenceIndex = 0; referenceIndex < references.length; referenceIndex++) {
			Reference reference = references[referenceIndex];
			matches[referenceIndex] = new Match(reference.resource, reference.offset, reference.length);
		}
		mSearchResult.addMatches(matches);

		return Status.OK_STATUS;
	}
}