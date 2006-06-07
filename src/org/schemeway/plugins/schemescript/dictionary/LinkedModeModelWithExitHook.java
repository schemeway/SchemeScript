/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import gnu.mapping.*;

import org.eclipse.jface.text.link.*;
import org.schemeway.plugins.schemescript.*;

public class LinkedModeModelWithExitHook extends LinkedModeModel {
	
	private Procedure mExitHook;
	
	public LinkedModeModelWithExitHook(Procedure exitHook) {
		mExitHook = exitHook;
	}
	
	public void exit(int flags) {
		super.exit(flags);
		if (mExitHook != null && (flags & ILinkedModeListener.EXIT_ALL) != 0) {
			try {
				mExitHook.apply0();
			}
			catch (Throwable e) {
				SchemeScriptPlugin.logException("Exit hook threw an exception", e);
			}
		}
	}
}
