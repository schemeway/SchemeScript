/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.resources.*;
import org.eclipse.jface.resource.*;
import org.eclipse.search.ui.*;
import org.eclipse.search.ui.text.*;
import org.eclipse.ui.*;

class SymbolSearchResult extends AbstractTextSearchResult implements IFileMatchAdapter,
		IEditorMatchAdapter {
	
	private SymbolSearchQuery mQuery;

	public SymbolSearchResult(SymbolSearchQuery query) {
		mQuery = query;
	}

	public IEditorMatchAdapter getEditorMatchAdapter() {
		return this;
	}

	public IFileMatchAdapter getFileMatchAdapter() {
		return this;
	}

	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	public String getLabel() {
		int count = getMatchCount();
		return mQuery.getLabel() + " - " + count + " match" + (count <= 1 ? "" : "es") ;
	}

	public ISearchQuery getQuery() {
		return mQuery;
	}

	public String getTooltip() {
		return null;
	}

	public Match[] computeContainedMatches(AbstractTextSearchResult result, IFile file) {
		return getMatches(file);
	}

	public IFile getFile(Object element) {
		if (element instanceof IFile)
			return (IFile) element;
		return null;
	}

	public Match[] computeContainedMatches(AbstractTextSearchResult result, IEditorPart editor) {
		IEditorInput ei = editor.getEditorInput();
		if (ei instanceof IFileEditorInput) {
			IFileEditorInput fi = (IFileEditorInput) ei;
			return getMatches(fi.getFile());
		}
		return new Match[0];
	}

	public boolean isShownInEditor(Match match, IEditorPart editor) {
		IEditorInput input = editor.getEditorInput();
		if (input instanceof IFileEditorInput) {
			IFileEditorInput fileEditorInput = (IFileEditorInput) input;
			return match.getElement().equals(fileEditorInput.getFile());
		}
		return false;
	}
}