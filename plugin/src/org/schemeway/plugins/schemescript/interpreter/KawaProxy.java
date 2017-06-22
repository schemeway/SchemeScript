/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import gnu.lists.*;
import gnu.mapping.*;
import kawa.standard.*;

import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;

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
	// Fix by Harry Pantazis, car and cdr are protected now. 
	public static Object get(final String symbolName) {
        final Pair box = new Pair();
        
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				try {
					Object object = Environment.getCurrent().get(symbolName);
					box.setCar(object);
				} catch (Throwable e) {
					SchemeScriptPlugin.logException("Error while referencing Scheme symbol", e);
					box.setCar(null);
				}
			}
		});
		return box.getCar();
	}
    
    // Sets a symbol in the global environment
    public static void set(final String symbolName, final Object value)
    {
        runInSchemeThread(new Runnable() {
            public void run() {
                Environment env = Environment.getCurrent();
                env.define(env.getSymbol(symbolName), null, value);
            }
        });
    }
	
	// Runs a runnable in the Scheme thread
	public static void runInSchemeThread(Runnable runnable) {
		if (runnable == null)
			return;
		
		Display.getDefault().syncExec(runnable);
	}
}
