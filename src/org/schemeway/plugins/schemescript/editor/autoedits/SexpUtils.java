/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.parser.*;

public final class SexpUtils {

    private SexpUtils() {
        // should not be instantiated
    }

    public static boolean whitespacesOnly(IDocument document, int offset, int length) throws BadLocationException {
        int end = offset + length;
        for (int i=offset; i<end; i++) {
            if (!Character.isWhitespace(document.getChar(i)))
                return false;
        }
        return true;
    }
    
    public static void deleteSelection(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        int start = command.offset;
        int end   = start + command.length;
        
        int index = end;
        while (navigator.upSexpression(index) && navigator.getSexpStart() >= start) {
            if (!navigator.forwardSexpression(navigator.getSexpStart())) {
                command.doit = false;
                return;
            }
            index = navigator.getSexpEnd();
        }
        if (end == index && navigator.forwardSexpression(end) && navigator.getSexpStart() < end) { 
            end = index = navigator.getSexpEnd();
        }
        end = index;

        while (index > start) {
            if (navigator.backwardSexpression(index) && navigator.getSexpEnd() > start)
                index = navigator.getSexpStart();
            else
                break;
        }
        start = index;
        
        // strip whitespaces, before and after...
        if (command.offset < start && whitespacesOnly(document, command.offset, start - command.offset))
            start = command.offset;
        
        if (end < command.offset + command.length && whitespacesOnly(document, end, (command.offset + command.length) - end))
            end = command.offset + command.length;

        command.offset = start;
        command.length = end - start;
    }

    public static void deleteForwardSexp(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        if (navigator.forwardSexpression(command.offset)) {
            int start = navigator.getSexpStart();
            int end   = navigator.getSexpEnd();
            command.length = end - start;
        }
    }

    public static void deleteBackwardSexp(IDocument document, DocumentCommand command) throws BadLocationException {
        SexpNavigator navigator = new SexpNavigator(document);
        if (navigator.backwardSexpression(command.offset + 1)) {
            int start = navigator.getSexpStart();
            int end   = navigator.getSexpEnd();
            command.offset = start;
            command.length = end - start;
        }
    }
}
