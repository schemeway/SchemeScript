/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import java.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.ui.texteditor.*;

/**
 * @author SchemeWay Project.
 */
public class SchemeAnnotationHover implements IAnnotationHover {

	/**
	 * @param editor
	 */
	public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber) {
		String hoverInfo = null;

		IAnnotationModel annotationModel = sourceViewer.getAnnotationModel();
		IDocument document = sourceViewer.getDocument();

		try {
			Iterator annotationIterator = annotationModel.getAnnotationIterator();
			StringBuffer info = new StringBuffer();

			while (annotationIterator.hasNext()) {
				Annotation annotation = (Annotation) annotationIterator.next();
				Position position = annotationModel.getPosition(annotation);

				if (isMarkerAnnotation(annotation) && isPositionSpansLine(lineNumber, document, position)) {
					if (info.length() > 0)
						info.append('\n');
					info.append(annotation.getText());
				}
			}

			hoverInfo = info.toString();
		}
		catch (BadLocationException e) {
		}

		return hoverInfo;
	}

	private boolean isMarkerAnnotation(Annotation annotation) {
		return annotation instanceof MarkerAnnotation;
	}

	private boolean isPositionSpansLine(int lineNumber, IDocument document, Position position)
			throws BadLocationException {
		return document.getLineOfOffset(position.offset) <= lineNumber
				&& lineNumber <= document.getLineOfOffset(position.offset + position.length);
	}
}
