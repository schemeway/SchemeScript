/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import org.eclipse.core.runtime.Path;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;

/**
 * @author SchemeWay Project.
 *
 */
public final class SchemeOutlineImageProvider {
	private static Image CHAPTER_IMAGE;
    private static Image SECTION_IMAGE;
    private static Image DEFINITION_IMAGE;
    
    static {
        try {
            CHAPTER_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.findFile(new Path("icons/chapter.gif")).openStream());
            SECTION_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.findFile(new Path("icons/section.gif")).openStream());
            DEFINITION_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.findFile(new Path("icons/definition.gif")).openStream());
        }
        catch (Throwable exception) {
            CHAPTER_IMAGE = null;
            SECTION_IMAGE = null;
            DEFINITION_IMAGE = null;
        }
    }

    public static Image getChapterImage() {
    	return CHAPTER_IMAGE;
    }
    
    public static Image getSectionImage() {
    	return SECTION_IMAGE;
    }
    
    public static Image getDefinitionImage() {
    	return DEFINITION_IMAGE;
    }
    
	private SchemeOutlineImageProvider() {
	}
}
