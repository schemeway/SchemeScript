/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import org.eclipse.jface.viewers.*;

class OutlineNodeContentProvider implements ITreeContentProvider {
    
    public Object[] getChildren(Object parentElement)
    {
        if (parentElement != null) {
            return ((OutlineNode)parentElement).getChildren();
        }
        return null;
    }
    public Object getParent(Object element)
    {
        return ((OutlineNode)element).parent;
    }
    
    public boolean hasChildren(Object element)
    {
        return ((OutlineNode)element).children.size() > 0;
    }
    
    public Object[] getElements(Object inputElement)
    {
        return getChildren(inputElement);
    }
    
    public void dispose()
    {
    }
    
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
    {
    }
}