/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.jface.viewers.*;

public class SymbolTableContentProvider implements IStructuredContentProvider {
	/**
	 * 
	 */
	private static final Object[] EMPTY_ARRAY = new Object[0];
	private SymbolSearchResult mSearchResult;
	private SymbolSearchResultPage mPage;

	/**
	 * @param symbolSearchResultPage
	 */
	public SymbolTableContentProvider(SymbolSearchResultPage symbolSearchResultPage) {
		mPage = symbolSearchResultPage;
	}

	public void dispose() {
		// nothing to do
	}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof SymbolSearchResult) {
			int elementLimit = mPage.getElementLimit().intValue();
			Object[] elements = ((SymbolSearchResult) inputElement).getElements();
			if (elementLimit != -1 && elements.length > elementLimit) {
				Object[] shownElements = new Object[elementLimit];
				System.arraycopy(elements, 0, shownElements, 0, elementLimit);
				return shownElements;
			}
			return elements;
		}
		return EMPTY_ARRAY;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		if (newInput instanceof SymbolSearchResult) {
			mSearchResult = (SymbolSearchResult) newInput;
		}
	}

	public void elementsChanged(Object[] updatedElements) {
		TableViewer viewer = (TableViewer) mPage.getViewer();
		int elementLimit = mPage.getElementLimit().intValue();
		boolean tableLimited = elementLimit != -1;
		for (int i = 0; i < updatedElements.length; i++) {
			if (mSearchResult.getMatchCount(updatedElements[i]) > 0) {
				if (viewer.testFindItem(updatedElements[i]) != null) {
					viewer.update(updatedElements[i], null);
				}
				else {
					if (!tableLimited || viewer.getTable().getItemCount() < elementLimit) {
						viewer.add(updatedElements[i]);
					}
				}
			}
			else {
				viewer.remove(updatedElements[i]);
			}
		}
	}

	public void clear() {
		mPage.getViewer().refresh();
	}

}