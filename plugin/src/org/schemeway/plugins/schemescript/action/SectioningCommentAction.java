/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.tools.*;

public class SectioningCommentAction extends SchemeAction {
    private boolean mSubsection;

    protected SectioningCommentAction(SchemeEditor editor, boolean subsection) {
        super(editor);
        mSubsection = subsection;
        setText(subsection ? "Insert section comment" : "Insert chapter comment");
        setToolTipText(subsection ? "Insert section comment" : "Insert chapter comment");
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;
        
        int point = editor.getPoint();
        String newline = TextUtilities.getDefaultLineDelimiter(editor.getDocument());

        if (mSubsection) {
            editor.insertText(point, Comments.createSectionComment(newline));
            editor.setPoint(point + 8 + newline.length());
        }
        else {
            editor.insertText(point, Comments.createChapterComment(newline));
            editor.setPoint(point + 15 + 2 * newline.length());
        }
    }

    public static SectioningCommentAction createChapterCommentAction(SchemeEditor editor) {
        return new SectioningCommentAction(editor, false);
    }

    public static SectioningCommentAction createSectionCommentAction(SchemeEditor editor) {
        return new SectioningCommentAction(editor, true);
    }
}