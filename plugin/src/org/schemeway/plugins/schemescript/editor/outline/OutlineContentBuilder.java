/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

/**
 * @author SchemeWay Project.
 *
 */
public interface OutlineContentBuilder {
	OutlineNode[] buildNodes(SchemeEditor editor) throws BadLocationException, BadPositionCategoryException; 
}
