/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import java.io.*;
import java.util.*;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.preference.*;
import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.parser.*;

public class PreferenceUtil {
    private final static String LIST_DELIMITER = "|";
    private final static char FIELD_DELIMITER = ',';

    private final static String[] PREFERENCES_EXTENSIONS = new String[] {
        "*.epf"
    };

    // -- Syntax keywords --

    public static String[] getKeywords(IPreferenceStore store, String preference) {
        Assert.isNotNull(store);
        Assert.isNotNull(preference);

        String keywords = store.getString(preference);
        if (keywords == null)
            return new String[0];

        StringTokenizer tokenizer = new StringTokenizer(keywords, LIST_DELIMITER);
        String[] result = new String[tokenizer.countTokens()];
        int index = 0;
        while (tokenizer.hasMoreTokens()) {
            result[index++] = tokenizer.nextToken();
        }
        return result;
    }

    public static void setKeywords(IPreferenceStore store, String preference, String[] keywords) {
        Assert.isNotNull(store);
        Assert.isNotNull(preference);
        Assert.isNotNull(keywords);
        store.setValue(preference, buildKeywordString(keywords));
    }

    public static void setDefaultKeywords(IPreferenceStore store, String preference, String[] keywords) {
        Assert.isNotNull(store);
        Assert.isNotNull(preference);
        Assert.isNotNull(keywords);
        store.setDefault(preference, buildKeywordString(keywords));
    }

    private static String buildKeywordString(String[] keywords) {
        StringBuffer buffer = new StringBuffer();
        for (int index = 0; index < keywords.length; index++) {
            buffer.append(keywords[index]).append(LIST_DELIMITER);
        }
        return buffer.toString();
    }

    public static void updateKeywordManager(IPreferenceStore store, KeywordManager keywordManager) {
        Assert.isNotNull(store);
        Assert.isNotNull(keywordManager);

        keywordManager.clear();
        String[] defines = getKeywords(store, SyntaxPreferences.SYNTAX_DEFINE);
        for (int i = 0; i < defines.length; i++)
            keywordManager.addDefine(defines[i]);
        String[] keywords = getKeywords(store, SyntaxPreferences.SYNTAX_KEYWORD);
        for (int i = 0; i < keywords.length; i++)
            keywordManager.addKeyword(keywords[i]);
        String[] specials = getKeywords(store, SyntaxPreferences.SYNTAX_SPECIAL);
        for (int i = 0; i < specials.length; i++)
            keywordManager.addSpecial(specials[i]);
        String[] mutators = getKeywords(store, SyntaxPreferences.SYNTAX_MUTATOR);
        for (int i = 0; i < mutators.length; i++)
            keywordManager.addMutator(mutators[i]);
        String[] constants = getKeywords(store, SyntaxPreferences.SYNTAX_CONSTANT);
        for (int i = 0; i < constants.length; i++)
            keywordManager.addConstant(constants[i]);
        keywordManager.setDefineRegularExpression(store.getString(SyntaxPreferences.SYNTAX_DEFINE_RE));
        keywordManager.setKeywordRegularExpression(store.getString(SyntaxPreferences.SYNTAX_KEYWORD_RE));
        keywordManager.setSpecialRegularExpression(store.getString(SyntaxPreferences.SYNTAX_SPECIAL_RE));
        keywordManager.setMutatorRegularExpression(store.getString(SyntaxPreferences.SYNTAX_MUTATOR_RE));
        keywordManager.setConstantRegularExpression(store.getString(SyntaxPreferences.SYNTAX_CONSTANT_RE));
    }

    // -- Indentation keyworkds
    public static IndentationRule[] getIndentationSchemes(IPreferenceStore store, String preference) {
        Assert.isNotNull(store);
        Assert.isNotNull(preference);

        String schemes = store.getString(preference);
        if (schemes == null)
            return new IndentationRule[0];

        StringTokenizer tokenizer = new StringTokenizer(schemes, LIST_DELIMITER);
        IndentationRule[] result = new IndentationRule[tokenizer.countTokens()];
        int index = 0;
        while (tokenizer.hasMoreTokens()) {
            result[index++] = parseIndentationScheme(tokenizer.nextToken());
        }
        return result;
    }

    private static IndentationRule parseIndentationScheme(String token) {
        int pos1 = token.indexOf(FIELD_DELIMITER);
        int pos2 = token.indexOf(FIELD_DELIMITER, pos1 + 1);

        if (pos1 >= 0 && pos2 > pos1) {
            String symbol = token.substring(0, pos1);
            String scheme = token.substring(pos1 + 1, pos2).intern();
            int hint = Integer.parseInt(token.substring(pos2 + 1));
            return new IndentationRule(symbol, scheme, hint);
        }
        return null;
    }

    public static void setIndentationSchemes(IPreferenceStore store, String preference, IndentationRule[] schemes) {
        Assert.isNotNull(store);
        Assert.isNotNull(preference);
        Assert.isNotNull(schemes);
        store.setValue(preference, buildSchemeString(schemes));
    }

    public static void setDefaultIndentationSchemes(IPreferenceStore store,
                                                    String preference,
                                                    IndentationRule[] schemes) {
        Assert.isNotNull(store);
        Assert.isNotNull(preference);
        Assert.isNotNull(schemes);
        store.setDefault(preference, buildSchemeString(schemes));
    }

    private static String buildSchemeString(IndentationRule[] schemes) {
        StringBuffer buffer = new StringBuffer();

        for (int index = 0; index < schemes.length; index++) {
            IndentationRule scheme = schemes[index];
            buffer.append(scheme.getSymbol()).append(FIELD_DELIMITER);
            buffer.append(scheme.getCategory()).append(FIELD_DELIMITER);
            buffer.append(scheme.getHint());
            buffer.append(LIST_DELIMITER);
        }
        return buffer.toString();
    }

    public static void updateIndentationManager(IPreferenceStore store, SchemeIndentationManager indentationManager) {
        Assert.isNotNull(store);
        Assert.isNotNull(indentationManager);
        indentationManager.clear();
        indentationManager.setRules(getIndentationSchemes(store, IndentationPreferences.INDENT_SCHEMES));
    }

    public static void importPreferences(Shell shell, IPreferenceStore store) {
        String filename = getPreferencesFilename(shell, "Import Scheme Preference...", SWT.OPEN);
        if (filename != null) {
            loadPreferencesFromFile(store, filename);
        }
    }

    public static void exportPreferences(Shell shell, IPreferenceStore store) {

        String filename = getPreferencesFilename(shell, "Export Scheme Preference...", SWT.SAVE);
        if (filename != null) {
            savePreferencesToFile(store, filename);
        }
    }

    private static String getPreferencesFilename(Shell shell, String title, int type) {
        FileDialog fileDialog = new FileDialog(shell, type);
        fileDialog.setText(title);
        fileDialog.setFilterExtensions(PREFERENCES_EXTENSIONS);
        return fileDialog.open();
    }

    private static void savePreferencesToFile(IPreferenceStore store, String filename) {
        try {
            FileWriter writer = new FileWriter(filename);
            PrintWriter printer = new PrintWriter(writer);
            try {
                formatPreferenceLine(printer, store, IndentationPreferences.INDENT_SCHEMES);
                formatPreferenceLine(printer, store, SyntaxPreferences.SYNTAX_DEFINE);
                formatPreferenceLine(printer, store, SyntaxPreferences.SYNTAX_KEYWORD);
                formatPreferenceLine(printer, store, SyntaxPreferences.SYNTAX_SPECIAL);
                formatPreferenceLine(printer, store, SyntaxPreferences.SYNTAX_MUTATOR);
                formatPreferenceLine(printer, store, SyntaxPreferences.SYNTAX_CONSTANT);
                formatPreferenceLine(printer, store, CommentPreferences.COMMENT_COPYRIGHT);
            }
            finally {
                printer.close();
            }
        }
        catch (IOException exception) {
            MessageDialog.openError(null, "File save error", "Unable to save Scheme preferences: "
                                                             + exception.getMessage());
        }
    }

    private static void loadPreferencesFromFile(IPreferenceStore store, String filename) {
        try {
            LineNumberReader reader = new LineNumberReader(new FileReader(filename));
            try {
                String line;
                while ((line = reader.readLine()) != null) {
                    extractPreference(store, line);
                }
            }
            finally {
                reader.close();
            }
        }
        catch (IOException exception) {
            MessageDialog.openError(null, "File save error", "Unable to save Scheme preferences: "
                                                             + exception.getMessage());
        }

    }

    private static void formatPreferenceLine(PrintWriter writer, IPreferenceStore store, String preferenceName) {
        writer.println(preferenceName + "=" + store.getString(preferenceName));
    }

    private static void extractPreference(IPreferenceStore store, String line) {
        int equalsIndex = line.indexOf('=');
        if (equalsIndex > 0 && equalsIndex < line.length() - 2) {
            String key = line.substring(0, equalsIndex).intern();
            String value = line.substring(equalsIndex + 1);
            store.putValue(key, value);
        }
    }
}