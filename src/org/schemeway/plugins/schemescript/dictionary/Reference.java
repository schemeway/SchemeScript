/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.resources.*;

public class Reference {
	public IResource resource;
	public int offset;
	public int length;

	public Reference(IResource mResource, int mOffset, int mLength) {
		length = mLength;
		offset = mOffset;
		resource = mResource;
	}
}