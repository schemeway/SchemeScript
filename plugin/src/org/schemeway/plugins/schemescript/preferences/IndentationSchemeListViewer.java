/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.preferences;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.indentation.*;


/**
 * @author SchemeWay Project.
 *
 */
public class IndentationSchemeListViewer extends TableViewer {

	/**
	 * @param table
	 */
	public IndentationSchemeListViewer(Table table) {
		super(table);
	}

	
	public void addIndentationRule(IndentationRule rule) {
		int index = indexForElement(rule);
		add(rule);
		getControl().setFocus();
		doSetSelection(new int[] {index});
	}
}
