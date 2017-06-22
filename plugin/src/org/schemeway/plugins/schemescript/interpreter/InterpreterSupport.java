/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.util.*;

import org.eclipse.core.runtime.*;
import org.schemeway.plugins.schemescript.*;

public class InterpreterSupport {
	
	private static final String EXTENSION_POINT_ID = SchemeScriptPlugin.PLUGIN_NS + ".interpreters";

    private InterpreterSupport() {
        // make sure this class is never instantiated
    }

    private static InterpreterType[] mCachedTypes = null;

    public static InterpreterType[] getTypes() {
        if (mCachedTypes != null) {
            return mCachedTypes;
        }

        IExtension[] extensions = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT_ID).getExtensions();
        
        mCachedTypes = parseTypes(extensions);
        return mCachedTypes;
    }

    private static InterpreterType[] parseTypes(IExtension[] extensions) {
        List<InterpreterType> types = new ArrayList<InterpreterType>(20);
        
        for (int i = 0; i < extensions.length; i++) {
            IExtension extension = extensions[i];
            IConfigurationElement[] elements = extension.getConfigurationElements();
            for (int j = 0; j < elements.length; j++) {
                IConfigurationElement element = elements[j];
                if (element.getName().equals("interpreter")) {
                    String name = element.getAttribute("name");
                    String id   = element.getAttribute("id");
                    String cls  = element.getAttribute("class");
                    InterpreterType type = new InterpreterType(name, id, cls, element);
                    
                    types.add(type);
                }
            }
        }
        
        return (InterpreterType[])types.toArray(new InterpreterType[types.size()]);
   }

}