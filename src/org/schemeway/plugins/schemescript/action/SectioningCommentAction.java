/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.tools.*;

public class SectioningCommentAction extends Action {
    private SchemeEditor mEditor;
    private boolean mSubsection;

    protected SectioningCommentAction(SchemeEditor editor, boolean subsection) {
        mEditor = editor;
        mSubsection = subsection;
        setText(subsection ? "Insert section comment" : "Insert chapter comment");
        setToolTipText(subsection ? "Insert section comment" : "Insert chapter comment");
    }

    public void run() {
        int point = mEditor.getPoint();
        String newline = TextUtilities.getDefaultLineDelimiter(mEditor.getDocument());

        if (mSubsection) {
            mEditor.insertText(point, Comments.createSectionComment(newline));
            mEditor.setPoint(point + 10 + newline.length());
        }
        else {
            mEditor.insertText(point, Comments.createChapterComment(newline));
            mEditor.setPoint(point + 15 + 2 * newline.length());
        }
    }

    public static SectioningCommentAction createChapterCommentAction(SchemeEditor editor) {
        return new SectioningCommentAction(editor, false);
    }

    public static SectioningCommentAction createSectionCommentAction(SchemeEditor editor) {
        return new SectioningCommentAction(editor, true);
    }
}