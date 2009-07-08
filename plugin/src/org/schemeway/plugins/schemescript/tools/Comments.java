/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.tools;

import org.eclipse.jface.preference.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class Comments {
    public static String createHeaderComment(String lineDelimiter) {
        return createHeaderComment(lineDelimiter, "");
    }

    public static String createHeaderComment(String lineDelimiter, String header) {
        StringBuffer buffer = new StringBuffer();

        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        String author = store.getString(CommentPreferences.COMMENT_AUTHOR);
        String copyright = store.getString(CommentPreferences.COMMENT_COPYRIGHT);

        buffer.append(";;;")
              .append(lineDelimiter)
              .append(";;;; ")
              .append(header)
              .append(lineDelimiter)
              .append(";;;")
              .append(lineDelimiter)
              .append(";;")
              .append(lineDelimiter)
              .append(";; @created   \"")
              .append(new java.util.Date())
              .append("\"")
              .append(lineDelimiter);
        if (author != null && author.length() > 0) {
            buffer.append(";; @author    \"").append(author).append("\"").append(lineDelimiter);
        }
        if (copyright != null && copyright.length() > 0) {
            buffer.append(";; @copyright \"").append(copyright).append("\"").append(lineDelimiter);
        }
        buffer.append(";;").append(lineDelimiter).append(lineDelimiter);

        return buffer.toString();
    }

    public static String createSectionComment(String lineDelimiter) {
        return createSectionComment(lineDelimiter, "");
    }

    public static String createSectionComment(String lineDelimiter, String header) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(";;;")
              .append(lineDelimiter)
              .append(";;;; ")
              .append(header)
              .append(lineDelimiter)
              .append(";;;")
              .append(lineDelimiter)
              .append(lineDelimiter);

        return buffer.toString();
    }

    public static String createChapterComment(String lineDelimiter) {
        return createChapterComment(lineDelimiter, "");
    }

    public static String createChapterComment(String lineDelimiter, String header) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(";;;")
              .append(lineDelimiter)
              .append(";;;; --")
              .append(lineDelimiter)
              .append(";;;; ")
              .append(header)
              .append(lineDelimiter)
              .append(";;;")
              .append(lineDelimiter)
              .append(lineDelimiter);

        return buffer.toString();
    }
}