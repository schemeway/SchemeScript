/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import gnu.lists.Pair;
import gnu.mapping.Environment;
import kawa.standard.load;

import org.eclipse.swt.widgets.Display;
import org.schemeway.plugins.schemescript.SchemeScriptPlugin;

/**
 * The KawaProxy help ensure that all Scheme evaluations are done in the same
 * thread. 
 * 
 * TODO - change the implementation to not use the UI thread 
 * @author schemeway
 */
public final class KawaProxy {

	private KawaProxy() {
		// Ensure that the class will not be instantiated
	}
	
	// Loads a file in the Kawa interpreter.
	public static void loadFile(final String filename) {
    	Display.getDefault().syncExec(new Runnable() {
    		public void run() {
    	        try {
    	            load.load.apply1(filename);
    	        }
    	        catch (Throwable e) {
    	            SchemeScriptPlugin.logException("Unable to load initialization files", e);
    	        }
    		}
    	});
	}
	
	// Dereferences a symbol in the global environment.
	public static Object get(final String symbolName) {
        final Pair box = new Pair();

		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				try {
					Object object = Environment.getCurrent().get(symbolName);
					box.car = object;
				} catch (Throwable e) {
					SchemeScriptPlugin.logException("Error while referencing Scheme symbol", e);
					box.car = null;
				}
			}
		});
		return box.car;
	}
	
	// Runs a runnable in the Scheme thread
	public static void runInSchemeThread(Runnable runnable) {
		if (runnable == null)
			return;
		
		Display.getDefault().syncExec(runnable);
	}
}
