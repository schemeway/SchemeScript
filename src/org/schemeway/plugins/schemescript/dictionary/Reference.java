/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.core.resources.IResource;

public class Reference {
	public IResource resource;
	public int offset;
	public int length;

	public Reference(IResource resource, int offset, int length) {
		super();
		this.length = length;
		this.offset = offset;
		this.resource = resource;
	}
}