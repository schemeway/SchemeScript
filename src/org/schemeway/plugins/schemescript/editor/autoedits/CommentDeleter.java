/*
 * Copyright (c) 2005 SchemeWay.com
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

/**
 * @author SchemeWay.com
 */
public class CommentDeleter implements IAutoEditStrategy {

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        if (command.text.length() != 0)
            return;
        if (command.length != 1)
            return;

        try {
            ITypedRegion partition = document.getPartition(command.offset);
            if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                if (command.offset == partition.getOffset()) {
                    command.text = "";
                    command.offset = partition.getOffset();
                    command.length = partition.getLength();
                }
                else if (document.getChar(partition.getOffset()) == '#'
                         && ((command.offset - partition.getOffset()) <= 1 
                            || (partition.getOffset() + partition.getLength() - command.offset) <= 2)) {
                    command.offset = partition.getOffset();
                    command.length = partition.getLength();
                }
            }
        }
        catch (BadLocationException exception) {
        }
    }
}