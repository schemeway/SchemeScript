/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript;

import java.util.*;

import kawa.standard.*;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.util.*;
import org.eclipse.ui.plugin.*;
import org.osgi.framework.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.interpreter.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

/**
 * The main plugin class to be used in the desktop.
 */
public class SchemeScriptPlugin extends AbstractUIPlugin {
    public final static String PLUGIN_NS = "org.schemeway.plugins.schemescript";
    
    //The shared instance.
    private static SchemeScriptPlugin plugin;
    //Resource bundle.
    private ResourceBundle resourceBundle;

    private SchemeTextTools textTools;
    private Interpreter     interpreter;
    
    IPropertyChangeListener propertyChangedListener = null;
    
    /**
     * The constructor.
     */
    public SchemeScriptPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle.getBundle("resources");
        }
        catch (MissingResourceException x) {
            resourceBundle = null;
        }
    }

    /**
     * This method is called upon plug-in activation
     */
    public void start(BundleContext context) throws Exception {
        super.start(context);

        Scheme.registerEnvironment();
        textTools = new SchemeTextTools(new ColorManager());
        
        if (propertyChangedListener == null) {
            propertyChangedListener = new IPropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent event) {
                    IPreferenceStore store = getPreferenceStore();
                    if (event.getProperty().startsWith(SchemeLexicalExtensionsPreferences.PREFIX)) {
                        initializeScanner(store);
                    }
                }
            };
            getPreferenceStore().addPropertyChangeListener(propertyChangedListener);
        }
    }

    /**
     * This method is called when the plug-in is stopped
     */
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
        if (propertyChangedListener != null) {
            getPreferenceStore().removePropertyChangeListener(propertyChangedListener);
        }
    }

    /**
     * Returns the shared instance.
     */
    public static SchemeScriptPlugin getDefault() {
        return plugin;
    }

    /**
     * Returns the string from the plugin's resource bundle, or 'key' if not
     * found.
     */
    public static String getResourceString(String key) {
        ResourceBundle bundle = SchemeScriptPlugin.getDefault().getResourceBundle();
        try {
            return (bundle != null) ? bundle.getString(key) : key;
        }
        catch (MissingResourceException e) {
            return key;
        }
    }

    /**
     * Returns the plugin's resource bundle,
     */
    public ResourceBundle getResourceBundle() {
        return resourceBundle;
    }

    public SchemeTextTools getTextTools() {
        return textTools;
    }
    
    
    public Interpreter getInterpreter() {
        if (interpreter == null) {
            interpreter = createInterpreter();
        }
        return interpreter;
    }
    
    protected Interpreter createInterpreter() {
        return new BaseInterpreter();
    }
  
    
    public static void logException(String message, Throwable exception) {
        IStatus status = new Status(IStatus.ERROR, 
                                    getDefault().getBundle().getSymbolicName(),
                                    IStatus.OK,
                                    message,
                                    exception);
        getDefault().getLog().log(status);
    
    }

    protected void initializeDefaultPreferences(IPreferenceStore store) {
        SchemePreferences.initializeDefaults(store);
        CommentPreferences.initializeDefaults(store);
        ColorPreferences.initializeDefaults(store);
        SyntaxPreferences.initializeDefaults(store);
        IndentationPreferences.initializeDefaults(store);
        SchemeLexicalExtensionsPreferences.initializeDefaults(store);
        initializeScanner(store);
    }
    
    private void initializeScanner(IPreferenceStore store) {
        SchemeScannerUtilities.setBracketsAreParentheses(store.getBoolean(SchemeLexicalExtensionsPreferences.SQUARE_BRACKETS));
        SchemeScannerUtilities.setDashInIdentifiers(store.getBoolean(SchemeLexicalExtensionsPreferences.DASH_IN_IDS));
    }
}
