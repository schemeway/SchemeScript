/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.*;

import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class MouseCopyAction extends Action implements MouseMoveListener {
    private SchemeEditor mEditor;
    private int mCurrentMouseX;
    private int mCurrentMouseY;

    public MouseCopyAction(SchemeEditor editor, StyledText textWidget) {
        Assert.isNotNull(editor);
        Assert.isNotNull(textWidget);
        setText("Copies the S-expression near the mouse");
        setToolTipText("Copies the S-expression near the mouse");
        textWidget.addMouseMoveListener(this);
        mEditor = editor;
    }

    public void run() {
        Region selection = mEditor.getSelection();
        SexpExplorer explorer = mEditor.getExplorer();
        IDocument document = mEditor.getDocument();

        try {
            int mouseOffset = mEditor.getOffset(mCurrentMouseX, mCurrentMouseY);
            IRegion lineInfo = document.getLineInformationOfOffset(mouseOffset);
            int lineStart = lineInfo.getOffset();

            if (document.getPartition(mouseOffset).getType() == SchemePartitionScanner.SCHEME_COMMENT)
                return;

            if (mouseOffset >= 0 && mouseOffset < mEditor.getDocument().getLength()) {
                char charAfter = mEditor.getChar(mouseOffset);
                while (mouseOffset >= lineStart && (charAfter == '\r' || charAfter == '\n')) {
                    mouseOffset--;
                    charAfter = mEditor.getChar(mouseOffset);
                }

                if (!Character.isWhitespace(charAfter)) {
                    if (explorer.forwardSexpression(mouseOffset) && explorer.backwardSexpression(explorer.getSexpEnd())) {
                        mouseCopy(explorer.getText(), selection);
                    }
                    else {
                        if (explorer.backwardSexpression(mouseOffset + 1)
                            && explorer.forwardSexpression(explorer.getSexpStart())) {
                            mouseCopy(explorer.getText(), selection);
                        }
                    }
                }
            }
        }
        catch (Exception exception) {
        }
    }

    private void mouseCopy(String textToInsert, Region selection) {
        int offset = selection.getOffset();
        if (offset > 0) {
            char ch = mEditor.getChar(offset - 1);
            if (!Character.isWhitespace(ch) && ch != '(') {
                textToInsert = " " + textToInsert;
            }
        }
        mEditor.replaceText(offset, selection.getLength(), textToInsert);
        mEditor.setPoint(offset + textToInsert.length());
    }

    public void mouseMove(MouseEvent e) {
        mCurrentMouseX = e.x;
        mCurrentMouseY = e.y;
    }
}