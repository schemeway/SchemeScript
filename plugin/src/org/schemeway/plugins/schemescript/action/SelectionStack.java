/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.action;

import java.util.*;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.*;

public class SelectionStack {
    private List<Region> mStack; 
    
    public SelectionStack() {
        mStack = new LinkedList<Region>();
    }
    
    public void push(Region selection, Region previousSelection){
        Assert.isNotNull(selection);
        if (mStack.size() > 0) {
            if (previousSelection == null || !previousSelection.equals(mStack.get(0))) {
                mStack.clear();
            }
        }
        mStack.add(0, selection);
    }
    
    public Region pop() {
        if (mStack.size() > 0) {
            Region element = (Region)mStack.get(0);
            mStack.remove(0);
            return element;
        }
        return null;
    }
    
    public void clear() {
        mStack.clear();
    }
}
