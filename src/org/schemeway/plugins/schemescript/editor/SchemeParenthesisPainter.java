/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;

import org.schemeway.plugins.schemescript.parser.*;

public class SchemeParenthesisPainter implements IPainter, PaintListener {
    private Position mBracketPosition = new Position(0, 0);
    private SchemeEditor mEditor;
    
    private boolean mIsActive = false;
    private ISourceViewer mSourceViewer;
    private StyledText mTextWidget;
    private Color mDefaultColor = new Color(null, 0, 0, 0);
    private Color mMismatchColor = new Color(null, 196, 0, 0);
    private Color mColor;
    private boolean mBox;
    private boolean mMismatch = false;
    private IPaintPositionManager mPositionManager;

    /**
     * Constructor
     * 
     * @param sourceViewer Source in which to paint brackets.
     */
    public SchemeParenthesisPainter(final ISourceViewer sourceViewer, SchemeEditor editor) {
        mEditor = editor;
        mSourceViewer = sourceViewer;
        mTextWidget = sourceViewer.getTextWidget();
    }

    /**
     * @see IPainter#setHighlightColor
     */
    public final void setHighlightColor(final Color color) {
        mColor = color;
    }
    
    public final void setParenthesisColor(Color color) {
        mDefaultColor = color;
    }

    /**
     * @see IPainter#setHighlightStyle
     */
    public final void setHighlightStyle(final boolean box) {
        mBox = box;
    }

    /**
     * @see IPainter#dispose
     */
    public void dispose() {
        mColor = null;
        mTextWidget = null;
    }

    /**
     * @see IPainter#deactivate
     */
    public final void deactivate(final boolean redraw) {
        if (mIsActive) {
            mIsActive = false;
            mTextWidget.removePaintListener(this);
            if (mPositionManager != null) {
                mPositionManager.unmanagePosition(mBracketPosition);
            }
            if (redraw) {
                handleDrawRequest(null);
            }
        }
    }

    /**
     * @see IPainter#paintControl
     */
    public final void paintControl(final PaintEvent event) {
        if (mTextWidget != null) {
            handleDrawRequest(event.gc);
        }
    }

    /**
     * Internal draw request handler.
     * 
     * @param gc Graphics context to update.
     */
    private final void handleDrawRequest(final GC gc) {
        if (mBracketPosition.isDeleted) {
            return;
        }

        int length = mBracketPosition.getLength();
        if (length < 1) {
            return;
        }

        int offset = mBracketPosition.getOffset();
        IRegion region = mSourceViewer.getVisibleRegion();

        if (region.getOffset() <= offset && region.getOffset() + region.getLength() >= offset + length) {
            offset -= region.getOffset();
            draw(gc, offset, 1);
        }
    }

    /**
     * @see IPainter#draw
     */
    private final void draw(final GC gc, final int offset, final int length) {
        if (gc != null) {
            Point left = mTextWidget.getLocationAtOffset(offset);
            Point right = mTextWidget.getLocationAtOffset(offset + length);
            Color color = mMismatch ? mMismatchColor : mColor;

            if (mBox) {
                gc.setForeground(color);
                int x = left.x;
                int y = left.y;
                int w = right.x - left.x - 1;
                int h = gc.getFontMetrics().getHeight();
                gc.drawRectangle(x, y, w, h);
            }
            else {
                gc.setForeground(mDefaultColor);
                gc.setBackground(color);
                gc.drawString(mTextWidget.getTextRange(offset, 1), left.x, left.y, false);
            }
        }
        else {
            mTextWidget.redrawRange(offset, length, true);
        }
    }

    /**
     * @see IPainter#paint(int)
     */
public final void paint(final int reason) {
        Point selection = mSourceViewer.getSelectedRange();
        if (selection.y > 0) {
            deactivate(true);
            return;
        }
        
        SexpNavigator explorer = mEditor.getExplorer();
        
        boolean backward = true;
        boolean closeToParen = false;
        int offset = selection.x;
        IDocument document = mEditor.getDocument();
        try {
            char previousChar = '\0';
            char nextChar = '\0';
            
            if (selection.x > 0)
                previousChar = document.getChar(selection.x - 1);
            
            if (selection.x > 0
                && SchemeScannerUtilities.isClosingParenthesis(previousChar)
                && SchemeTextUtilities.getPartition(document, selection.x - 1).getType() == IDocument.DEFAULT_CONTENT_TYPE) {
                closeToParen = true;
            }
            else {
                nextChar = document.getChar(selection.x);
                if (selection.x < document.getLength() - 1
                    && SchemeScannerUtilities.isOpeningParenthesis(nextChar)
                    && SchemeTextUtilities.getPartition(document, selection.x).getType() == IDocument.DEFAULT_CONTENT_TYPE) {
                    closeToParen = true;
                    backward = false;
                }
            }

            if (closeToParen && backward && explorer.backwardSexpression(selection.x)) {
                offset = explorer.getListStart();
                char matchingChar = document.getChar(offset);
                mMismatch = SchemeScannerUtilities.getParenthesisType(previousChar) != SchemeScannerUtilities.getParenthesisType(matchingChar);
            }
            else {
                if (closeToParen && !backward && explorer.forwardSexpression(selection.x)) {
                    offset = explorer.getSexpEnd() - 1;
                    char matchingChar = document.getChar(offset);
                    mMismatch = SchemeScannerUtilities.getParenthesisType(nextChar) != SchemeScannerUtilities.getParenthesisType(matchingChar);
                }
                else {
                    deactivate(true);
                    return;
                }
            }

        }
        catch (BadLocationException exception) {
            deactivate(true);
            return;
        }

        if (mIsActive) {
            // only if different
            if (offset != mBracketPosition.getOffset()) {
                // remove old highlighting
                handleDrawRequest(null);
                // update position
                mBracketPosition.isDeleted = false;
                mBracketPosition.offset = offset;
                mBracketPosition.length = 1;
                // apply new highlighting
                handleDrawRequest(null);
            }
        }
        else {
            mIsActive = true;

            mBracketPosition.isDeleted = false;
            mBracketPosition.offset = offset;
            mBracketPosition.length = 1;

            mTextWidget.addPaintListener(this);
            mPositionManager.managePosition(mBracketPosition);
            handleDrawRequest(null);
        }
    }

    /**
     * @see IPainter#setPositionManager(IPaintPositionManager)
     */
    public void setPositionManager(final IPaintPositionManager manager) {
        mPositionManager = manager;
    }
}