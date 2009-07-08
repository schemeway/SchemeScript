/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.search.ui.text.*;
import org.eclipse.ui.*;
import org.eclipse.ui.model.*;
import org.schemeway.plugins.schemescript.*;

/**
 * @author SchemeWay Project.
 *
 */
public class SymbolSearchResultPage extends AbstractTextSearchViewPage {

	private SymbolTableContentProvider mContentProvider;
	
	public SymbolSearchResultPage() {
		super(AbstractTextSearchViewPage.FLAG_LAYOUT_FLAT);
		setElementLimit(new Integer(100));
	}
	
	protected void clear() {
	}

	protected void configureTableViewer(TableViewer viewer) {
		mContentProvider = new SymbolTableContentProvider(this);
		viewer.setContentProvider(mContentProvider);
		viewer.setLabelProvider(new WorkbenchLabelProvider());
	}

	protected void configureTreeViewer(TreeViewer viewer) {
	}

	protected void elementsChanged(Object[] objects) {
		mContentProvider.elementsChanged(objects);
	}	
	
	public StructuredViewer getViewer() {
		return super.getViewer();
	}
	
	protected void showMatch(Match match, int currentOffset, int currentLength) throws PartInitException {
		Object element = match.getElement();
		if (element instanceof IFile) {
			IFile file = (IFile) element;
			SchemeScriptTools.openEditor(file, currentOffset, currentLength);
		}
	}
}
