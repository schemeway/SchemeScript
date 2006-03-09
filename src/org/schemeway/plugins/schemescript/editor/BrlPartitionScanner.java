/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.rules.Token;

/**
 * @author SchemeWay Project.
 *
 */
public class BrlPartitionScanner extends SchemePartitionScanner {

	public BrlPartitionScanner() {
		super();
	}
	
	protected Token scanToken() {
		if (getPosition() == 0 || lookahead() == ']') {
			if (getPosition() != 0) 
				consume();
			return scanBrlString();
		}
		else
			return super.scanToken();
	}
	
	protected boolean endOfDefaultPartition() {
		return isEndPosition(getPosition()) || (lookahead() == ']');
	}
	
	private Token scanBrlString() {
		while (!isEndPosition(getPosition()) && lookahead() != '[') {
			consume();
		}
		if (lookahead() == '[') {
			consume();
		}
		return SchemePartitionScanner.TOKEN_STRING;
	}
}
