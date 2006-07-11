/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.text.*;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class MouseCopyAction extends SchemeAction implements MouseMoveListener {
    private int mCurrentMouseX;
    private int mCurrentMouseY;
    private boolean mAddLeadingPunctuation;

    public MouseCopyAction(SchemeEditor editor, StyledText textWidget, boolean addLeadingPunctuation) {
        super(editor);
        Assert.isNotNull(textWidget);
        setText("Copies the S-expression near the mouse");
        setToolTipText("Copies the S-expression near the mouse");
        textWidget.addMouseMoveListener(this);
        mAddLeadingPunctuation = addLeadingPunctuation;
    }

    public void run() {
        SchemeEditor editor = getSchemeEditor();
        if (editor == null)
            return;

        Region selection = editor.getSelection();
        SexpNavigator explorer = editor.getExplorer();
        IDocument document = editor.getDocument();

        try {
            int mouseOffset = editor.getOffset(mCurrentMouseX, mCurrentMouseY);
            IRegion lineInfo = document.getLineInformationOfOffset(mouseOffset);
            int lineStart = lineInfo.getOffset();

            if (SchemeTextUtilities.getPartition(document, mouseOffset).getType() == SchemePartitionScanner.SCHEME_COMMENT)
                return;

            if (mouseOffset >= 0 && mouseOffset < editor.getDocument().getLength()) {
                char charAfter = editor.getChar(mouseOffset);
                while (mouseOffset >= lineStart && (charAfter == '\r' || charAfter == '\n')) {
                    mouseOffset--;
                    charAfter = editor.getChar(mouseOffset);
                }

                if (!Character.isWhitespace(charAfter)) {
                    if (explorer.forwardSexpression(mouseOffset) && explorer.backwardSexpression(explorer.getSexpEnd())) {
                        String text = explorer.getText();
                        int textStart = explorer.getSexpStart();
                        if (textStart > 0 && mAddLeadingPunctuation) {
                            char ch = editor.getChar(textStart - 1);
                            if (SchemeScannerUtilities.isPunctuationChar(ch) && !SchemeScannerUtilities.isClosingParenthesis(ch)) {
                                text = ch + text;
                            }
                        }
                        mouseCopy(text, selection, editor);
                    }
                    else {
                        if (explorer.backwardSexpression(mouseOffset + 1)
                            && explorer.forwardSexpression(explorer.getSexpStart())) {
                            mouseCopy(explorer.getText(), selection, editor);
                        }
                    }
                }
            }
        }
        catch (Exception exception) {
        }
    }

    private void mouseCopy(String textToInsert, Region selection, SchemeEditor editor) {
        int offset = selection.getOffset();
        if (offset > 0) {
            char ch = editor.getChar(offset - 1);
            if (!Character.isWhitespace(ch) 
                  && (!SchemeScannerUtilities.isPunctuationChar(ch) 
                       || SchemeScannerUtilities.isClosingParenthesis(ch))) {
                textToInsert = " " + textToInsert;
            }
        }
        editor.replaceText(offset, selection.getLength(), textToInsert);
        editor.setPoint(offset + textToInsert.length());
    }

    public void mouseMove(MouseEvent e) {
        mCurrentMouseX = e.x;
        mCurrentMouseY = e.y;
    }
}