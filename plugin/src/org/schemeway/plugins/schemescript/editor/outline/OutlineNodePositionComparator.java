/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import java.util.*;

class OutlineNodePositionComparator implements Comparator {
    public int compare(Object o1, Object o2) {
        OutlineNode s1 = (OutlineNode)o1;
        OutlineNode s2 = (OutlineNode)o2;
        
        if (s1.position.offset < s2.position.offset)
            return -1;
        else if (s1.position.offset > s2.position.offset)
            return 1;
        else
            return 0;
    }
}