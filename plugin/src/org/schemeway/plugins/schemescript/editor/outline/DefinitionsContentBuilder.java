/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import java.util.*;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class DefinitionsContentBuilder implements OutlineContentBuilder {

	public OutlineNode[] buildNodes(SchemeEditor editor) throws BadLocationException, BadPositionCategoryException {
		List<OutlineNode> nodes = new ArrayList<OutlineNode>();

		ContentUtilities.addDefinitions(editor, nodes, true);
		Collections.sort(nodes, ContentUtilities.NODE_COMPARATOR);

		return (OutlineNode[]) nodes.toArray(new OutlineNode[nodes.size()]);
	}

}
