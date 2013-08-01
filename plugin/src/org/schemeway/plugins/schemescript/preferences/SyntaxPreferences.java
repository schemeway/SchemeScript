/*
 * Copyright (c) 2004 Nu Echo Inc.
 *
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.preferences;

import java.util.regex.*;

import org.eclipse.jface.preference.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.parser.*;

public class SyntaxPreferences extends SchemePreferencePage {
    public final static String PREFIX = SchemeScriptPlugin.PLUGIN_NS + ".syntax.";
    public final static String SYNTAX_KEYWORD = PREFIX + "keywords";
    public final static String SYNTAX_DEFINE = PREFIX + "define";
    public final static String SYNTAX_SPECIAL = PREFIX + "special";
    public final static String SYNTAX_MUTATOR = PREFIX + "mutator";
    public final static String SYNTAX_CONSTANT = PREFIX + "constant";
    public final static String SYNTAX_KEYWORD_RE = PREFIX + "keywords-re";
    public final static String SYNTAX_DEFINE_RE = PREFIX + "define-re";
    public final static String SYNTAX_SPECIAL_RE = PREFIX + "special-re";
    public final static String SYNTAX_MUTATOR_RE = PREFIX + "mutator-re";
    public final static String SYNTAX_CONSTANT_RE = PREFIX + "constant-re";

    private static class SyntaxCategoryWidgets {
        public List listbox;
        public ListViewer viewer;
        public Text reTextbox;

    	public SyntaxCategoryWidgets(List list, Text textbox, ListViewer listViewer) {
			super();
			this.listbox = list;
			viewer = listViewer;
			reTextbox = textbox;
		}
    }

    private SyntaxCategoryWidgets mDefineWidgets;
    private SyntaxCategoryWidgets mKeywordWidgets;
    private SyntaxCategoryWidgets mSpecialWidgets;
    private SyntaxCategoryWidgets mMutatorWidgets;
    private SyntaxCategoryWidgets mConstantWidgets;

    private final static String[] DEFAULT_DEFINES = new String[] {
                "define",
                "define*",
                "define-syntax",
                "define-macro",
                "define-macro*",
                "define-syntax*",
                "defmacro",
                "define-simple-class",
                "define-class",
                "define-private",
                "define-constant",
                "define-namespace",
                "module-name",
                "module-export",
                "module-static",
                "module-implements",
                "module-compile-options",
                "require",
                "define-record-type",
                "define-unit",
                "define-alias",
                "define-private-alias",
                "define-base-unit"
    };
    private final static String[] DEFAULT_KEYWORDS = new String[] {
                "#!key", "#!rest", "#!optional"
    };

    private final static String[] DEFAULT_SPECIALS = new String[] {
                "lambda",
                "begin",
                "if",
                "cond",
                "case",
                "do",
                "when",
                "unless",
                "and",
                "or",
                "let",
                "let*",
                "letrec",
                "let-values",
                "let*-values",
                "receive",
                "fluid-let",
                "make",
                "invoke",
                "invoke-static",
                "invoke-special",
                "field",
                "static-field",
                "slot-ref",
                "catch",
                "throw",
                "primitive-throw",
                "try-finally",
                "try-catch",
                "dynamic-wind",
                "future",
                "delay",
                "force",
                "synchronized",
                "library",
                "import",
                "export",
                "package*",
                "include*"
    };
    private final static String[] DEFAULT_MUTATORS = new String[] {
                "set!", "set-car!", "set-cdr!", "vector-set!", "string-set!", "slot-set!"
    };

    private final static String[] DEFAULT_CONSTANTS = new String[] {
                "#!eof", "#!null", "#!void"
    };

    @Override
    protected Control createContents(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        composite.setLayout(layout);
        layout.marginHeight = 0;
        layout.marginWidth = 0;

        new Label(composite, SWT.NONE).setText("Configure the symbol categories for syntax coloring.");

        TabFolder folder = new TabFolder(composite, SWT.NONE);
        GridData data = new GridData(GridData.FILL_BOTH);
        data.heightHint = 400;
        folder.setLayoutData(data);

        mDefineWidgets = createListControl(folder, "Define", "Defining forms");
        mKeywordWidgets = createListControl(folder, "Special names", "Special names");
        mSpecialWidgets = createListControl(folder, "Special forms", "Special forms");
        mMutatorWidgets = createListControl(folder, "Mutator", "Mutating special forms/functions");
        mConstantWidgets = createListControl(folder, "Constant", "Constants");

        initializeValues();
        return composite;
    }

    private SyntaxCategoryWidgets createListControl(TabFolder folder, String tabName, String toolTip) {
    	GridData data;

        TabItem item = new TabItem(folder, SWT.NULL);
        item.setText(tabName);
        item.setToolTipText(toolTip);

        Composite composite = new Composite(folder, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);
        item.setControl(composite);

        Composite addDeleteComposite = new Composite(composite, SWT.NONE);
        layout = new GridLayout();
        layout.numColumns = 3;
        addDeleteComposite.setLayout(layout);
        data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        data.horizontalSpan = 3;
        addDeleteComposite.setLayoutData(data);

        final Text text = new Text(addDeleteComposite, SWT.SINGLE | SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
        text.setLayoutData(data);

        final Button addButton = new Button(addDeleteComposite, SWT.NONE);
        addButton.setText("Add");
        addButton.setToolTipText("Add a new symbol");
        addButton.setEnabled(false);
        data = new GridData();
        data.horizontalSpan = 1;
        addButton.setLayoutData(data);

        final Button deleteButton = new Button(addDeleteComposite, SWT.NONE);
        deleteButton.setText("Delete");
        deleteButton.setToolTipText("Delete the selected symbols");
        deleteButton.setEnabled(false);
        data = new GridData();
        data.horizontalSpan = 1;
        deleteButton.setLayoutData(data);

        final List list = new List(composite, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);
        data = new GridData(GridData.FILL_BOTH);
        data.horizontalSpan = 3;
        list.setLayoutData(data);
        final ListViewer listViewer = new ListViewer(list);
        listViewer.setContentProvider(new ArrayContentProvider());
        listViewer.setSorter(new ViewerSorter());

        list.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteButton.setEnabled(list.getSelectionCount() > 0);
            }
        });

        text.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent event) {
                String symbol = text.getText();
                addButton.setEnabled(SchemeScannerUtilities.isIdentifier(symbol) && (list.indexOf(symbol) == -1));
            }
        });

        addButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                addEntry(text, addButton, listViewer);
            }
        });

        deleteButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                list.remove(list.getSelectionIndices());
            }
        });

        Label reLabel = new Label(composite, SWT.NONE);
        reLabel.setText("Regular expression:");
        data = new GridData(GridData.BEGINNING);
        data.horizontalSpan = 2;
        reLabel.setLayoutData(data);

        final Text reText = new Text(composite, SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL);
        reText.setLayoutData(data);

        reText.addKeyListener(new KeyAdapter() {
        	@Override
            public void keyReleased(KeyEvent e) {
        		String eventText = reText.getText();
        		try {
        			Pattern.compile(eventText);
        			setValid(true);
        			setErrorMessage(null);
        		}
        		catch (PatternSyntaxException exception) {
        			setValid(false);
        			setErrorMessage("Invalid regular expression");
        		}
        	}
        });

        return new SyntaxCategoryWidgets(list, reText, listViewer);
    }

    @Override
    protected void doPerformDefaults() {
        mDefineWidgets.viewer.setInput(DEFAULT_DEFINES);
        mDefineWidgets.reTextbox.setText("");
        mKeywordWidgets.viewer.setInput(DEFAULT_KEYWORDS);
        mKeywordWidgets.reTextbox.setText("");
        mSpecialWidgets.viewer.setInput(DEFAULT_SPECIALS);
        mSpecialWidgets.reTextbox.setText("");
        mMutatorWidgets.viewer.setInput(DEFAULT_MUTATORS);
        mMutatorWidgets.reTextbox.setText("");
        mConstantWidgets.viewer.setInput(DEFAULT_CONSTANTS);
        mConstantWidgets.reTextbox.setText("");
    }

    public static void initializeDefaults(IPreferenceStore store) {
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_DEFINE, DEFAULT_DEFINES);
        store.setDefault(SYNTAX_DEFINE_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_KEYWORD, DEFAULT_KEYWORDS);
        store.setDefault(SYNTAX_KEYWORD_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_SPECIAL, DEFAULT_SPECIALS);
        store.setDefault(SYNTAX_SPECIAL_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_MUTATOR, DEFAULT_MUTATORS);
        store.setDefault(SYNTAX_MUTATOR_RE, "");
        PreferenceUtil.setDefaultKeywords(store, SYNTAX_CONSTANT, DEFAULT_CONSTANTS);
        store.setDefault(SYNTAX_CONSTANT_RE, "");
    }

    @Override
    protected void initializeValues() {
        KeywordManager manager = SchemeScriptPlugin.getDefault().getTextTools().getKeywordManager();
        mDefineWidgets.viewer.setInput(manager.getDefines());
        mDefineWidgets.reTextbox.setText(manager.getDefineRE());
        mKeywordWidgets.viewer.setInput(manager.getKeywords());
        mKeywordWidgets.reTextbox.setText(manager.getKeywordRE());
        mSpecialWidgets.viewer.setInput(manager.getSpecials());
        mSpecialWidgets.reTextbox.setText(manager.getSpecialsRE());
        mMutatorWidgets.viewer.setInput(manager.getMutators());
        mMutatorWidgets.reTextbox.setText(manager.getMutatorsRE());
        mConstantWidgets.viewer.setInput(manager.getConstants());
        mConstantWidgets.reTextbox.setText(manager.getConstantsRE());

    }

    @Override
    protected void storeValues() {
    	KeywordManager manager = SchemeScriptPlugin.getDefault().getTextTools().getKeywordManager();

    	manager.clear();
    	manager.setDefines(mDefineWidgets.listbox.getItems());
    	manager.setDefineRegularExpression(mDefineWidgets.reTextbox.getText());
    	manager.setConstants(mConstantWidgets.listbox.getItems());
    	manager.setConstantRegularExpression(mConstantWidgets.reTextbox.getText());
    	manager.setKeywords(mKeywordWidgets.listbox.getItems());
    	manager.setKeywordRegularExpression(mKeywordWidgets.reTextbox.getText());
    	manager.setMutators(mMutatorWidgets.listbox.getItems());
    	manager.setMutatorRegularExpression(mMutatorWidgets.reTextbox.getText());
    	manager.setSpecials(mSpecialWidgets.listbox.getItems());
    	manager.setSpecialRegularExpression(mSpecialWidgets.reTextbox.getText());
    	manager.saveValues();
    }

    private void addEntry(final Text text, final Button addButton, final ListViewer listViewer) {
        listViewer.add(text.getText());
        text.setText("");
        addButton.setEnabled(false);
    }

}