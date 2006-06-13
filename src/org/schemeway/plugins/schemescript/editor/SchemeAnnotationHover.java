/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.source.*;

/**
 * @author SchemeWay Project.
 *
 */
public class SchemeAnnotationHover implements IAnnotationHover {

	private SchemeEditor mEditor;
	
	/**
	 * @param editor
	 */
	public SchemeAnnotationHover(SchemeEditor editor) {
		mEditor = editor;
	}

	public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber) {
		String hoverInfo = null;
		
		int effectiveLineNumber = lineNumber + 1;
		try {
			IMarker[] markers = mEditor.getFile().findMarkers(IMarker.MARKER, true, 0);
			StringBuffer info = new StringBuffer();
			
			for (int i = 0; i < markers.length; i++) {
				IMarker marker = markers[i];
				
				String message = (String) marker.getAttribute(IMarker.MESSAGE);
				Integer line = (Integer) marker.getAttribute(IMarker.LINE_NUMBER);
				if (message != null && line != null && line.intValue() == effectiveLineNumber)
				{
					if (info.length() > 0)
						info.append('\n');
					info.append(message);
				}
			}
			
			hoverInfo = info.toString();
		}
		catch (CoreException e) {
		}
		
		return hoverInfo;
	}
}
