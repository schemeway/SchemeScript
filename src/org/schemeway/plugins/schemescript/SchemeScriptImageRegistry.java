/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript;

import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

public final class SchemeScriptImageRegistry {

    private SchemeScriptImageRegistry() {
    }

    // finds an image in the Plugin
    public static Image getImage(String relativePath) {

        ImageDescriptor descriptor = getImageDescriptor(relativePath);
        if (descriptor == null)
            return null;
        return descriptor.createImage();
    }

    public static ImageDescriptor getImageDescriptor(String relativePath) {
        return ImageDescriptor.createFromURL(SchemeScriptPlugin.getDefault().find(new Path(relativePath)));
    }
}