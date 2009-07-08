/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;

public class NodeLabelProvider extends LabelProvider {
    public String getText(Object element) {
        return ((OutlineNode)element).name;
    }
    
    public Image getImage(Object element)
    {
        OutlineNode section = (OutlineNode)element;
        if (section.type == OutlineNode.SECTION) {
            return SchemeOutlineImageProvider.getSectionImage();
        }
        else if (section.type == OutlineNode.CHAPTER ) {
            return SchemeOutlineImageProvider.getChapterImage();
        }
        
        return SchemeOutlineImageProvider.getDefinitionImage();
    }
}