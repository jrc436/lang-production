///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-5 University of Edinburgh (Michael White) and Gunes Erkan
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//////////////////////////////////////////////////////////////////////////////

package grammar;

import hylo.HyloHelper;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import lexicon.DefaultTokenizer;
import lexicon.Lexicon;
import lexicon.Tokenizer;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;
import org.jdom.transform.JDOMResult;
import org.jdom.transform.JDOMSource;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLFilter;
import org.xml.sax.XMLReader;

import synsem.LF;

/**
 * A CCG grammar is essentially a lexicon plus a rule group.
 * A grammar may also have sequences of transformations to use in 
 * loading/saving LFs from/to XML.
 *
 * @author  Michael White
 * @author  Gunes Erkan
 * @version $Revision: 1.45 $, $Date: 2010/12/06 02:39:35 $ 
 */
public class Grammar {

	//prefs
	public boolean showFeats = false; 
    //Whether to show semantic info (logical forms) 
    public boolean showSem = false;
    //Which features to show. 
    public String featsToShow = "";
		

    public final Lexicon lexicon; 
    public final RuleGroup rules;
    public final Types types;
    public final Set<String> supertagFeatures = new HashSet<String>(); //feats to include in supertags
    
    //I don't understand this.
    public final URL[] fromXmlTransforms; //xml to LF transforms   
    public final URL[] toXmlTransforms; //LFs to xml transforms
    
    // name of the grammar
    private String grammarName = null;

    // XML factories
    private SAXParserFactory spf = null; 
    private static SAXTransformerFactory stf = null; 
    
    // transformer for loading/saving LFs from/to XML
    private Transformer transformer = null;
    
    // transformations for loading/saving LFs from/to XML
    private Templates[] fromXmlTemplates = null;
    private Templates[] toXmlTemplates = null;
    
    // transformer for saving strings to APML
    private Transformer apmlTransformer = null;
    
    /** The pitch accents recognized as underscored suffixes for translation to APML. */
    public static final String[] pitchAccents = { 
        "H*", "L*", "L+H*", "L*+H", "H*+L", "H+L*"
    };

    // set of pitch accents
    private static Set<String> pitchAccentsSet = null;    
    
    /** The boundary tones recognized as separate tokens for translation to APML. */
    public static final String[] boundaryTones = { 
        "L", "H", "LL%", "HH%", "LH%", "HL%"
    };
    
    // set of boundary tones
    private static Set<String> boundaryTonesSet = null;    

    
    /** Loads a grammar from the given filename. */
    public Grammar(String filename) throws IOException {
        this(new File(filename).toURI().toURL());
    }
    
    /** Loads a grammar from the given URL. */
	public Grammar(URL url) throws IOException {
    	this(url, false);
    }
    
    /** Loads a grammar from the given URL, with the given flag for whether to ignore rule combos. */
    @SuppressWarnings("unchecked")
	public Grammar(URL url, boolean ignoreCombos) throws IOException {
        // read XML
        SAXBuilder builder = new SAXBuilder();
        Document doc;
        try {
            doc = builder.build(url);
        } catch (JDOMException jde) {
            throw (IOException) new IOException().initCause(jde);
        }
        Element root = doc.getRootElement();	// root corresponds to <grammar>
        grammarName = root.getAttributeValue("name");
		
        Element supertagsElt = root.getChild("supertags");
        if (supertagsElt != null) {
            String feats = supertagsElt.getAttributeValue("feats");
            if (feats != null) {
                String[] names = feats.split("\\s+");
                for (int i = 0; i < names.length; i++) {
                    supertagFeatures.add(names[i]);
                }
            }
        }
        if (supertagFeatures.isEmpty()) {
            // default is "form" and "lex"
            supertagFeatures.add("form"); supertagFeatures.add("lex"); 
        }
        
        Tokenizer tokenizer = initTokenizer(root.getChild("tokenizer"));       
        
        Element typesElt = root.getChild("types");
        URL typesUrl;
        if (typesElt != null) {
            typesUrl = new URL(url, typesElt.getAttributeValue("file"));
        }
        else {
        	typesUrl = null;
        }
        Element lexiconElt = root.getChild("lexicon");
        boolean openlex = "true".equals(lexiconElt.getAttributeValue("openlex"));
        URL lexiconUrl = new URL(url, lexiconElt.getAttributeValue("file")); 
        Element morphElt = root.getChild("morphology");
        URL morphUrl = new URL(url, morphElt.getAttributeValue("file"));
        Element rulesElt = root.getChild("rules");
        URL rulesUrl = new URL(url, rulesElt.getAttributeValue("file"));
        Element fromXmlElt = root.getChild("LF-from-XML");
        if (fromXmlElt != null) {
            List<Element> children = fromXmlElt.getChildren();
            fromXmlTransforms = new URL[children.size()];
            for (int i = 0; i < children.size(); i++) {
                Element transformElt = (Element) children.get(i);
                fromXmlTransforms[i] = new URL(url, transformElt.getAttributeValue("file"));
            }
        } else {
            fromXmlTransforms = new URL[0];
        }
        Element toXmlElt = root.getChild("LF-to-XML");
        if (toXmlElt != null) {
            List<Element> children = toXmlElt.getChildren();
            toXmlTransforms = new URL[children.size()];
            for (int i = 0; i < children.size(); i++) {
                Element transformElt = (Element) children.get(i);
                toXmlTransforms[i] = new URL(url, transformElt.getAttributeValue("file"));
            }
        } else {
            toXmlTransforms = new URL[0];
        }
        
        // load type hierarchy, lexicon and rules
        if (typesUrl != null) {
        	types = new Types(typesUrl, this);
        }
        else types = new Types(this);
    	lexicon = new Lexicon(this, tokenizer);
        
        lexicon.openlex = openlex;
        
        
        lexicon.init(lexiconUrl, morphUrl); 
        rules = new RuleGroup(rulesUrl, this);
        
        // add observed supertag-rule combos for filtering, if any, unless ignoring combos
        if (!ignoreCombos) {
	        String combosfile = rulesElt.getAttributeValue("combosfile");
	        if (combosfile != null) {
	        	URL combosUrl = new URL(url, combosfile);
	        	rules.loadSupercatRuleCombos(combosUrl);
	        }
	        // set dynamic combos: defaults to true with a combosfile, otherwise defaults to false
	        boolean dynamic = (combosfile != null);
	        String dynamicCombos = rulesElt.getAttributeValue("dynamic-combos");
	        if (dynamicCombos != null) dynamic = Boolean.parseBoolean(dynamicCombos);
	        rules.setDynamicCombos(dynamic);
        }
    }
    private Tokenizer initTokenizer(Element tokenizerElt) {
    	Tokenizer tokenizer = null;
    	if (tokenizerElt == null) {
    		return new DefaultTokenizer();
    	}
		String tokenClass = tokenizerElt.getAttributeValue("classname");	
		try {
			tokenizer = tokenClass == null ? new DefaultTokenizer() : (Tokenizer) Class.forName(tokenClass).newInstance();
		} 
		catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
			e.printStackTrace();
			System.err.println(tokenClass+" is not a valid classname, apparently");
			System.exit(1);
		}       
    	String repClasses = tokenizerElt.getAttributeValue("replacement-sem-classes");
        String[] semClasses = repClasses == null ? new String[0] : repClasses.split("\\s+");
        for (String cl : semClasses) {
            tokenizer.addReplacementSemClass(cl);
        }  	
    	return tokenizer;
    }
    
    /**
     * Returns a file url string relative to the user's current directory 
     * for the given filename.
     */
    public static String convertToFileUrl(String filename) {
        try {
            return new File(filename).toURI().toURL().toString();
        }
        catch (java.net.MalformedURLException exc) {
            throw (RuntimeException) new RuntimeException().initCause(exc);
        }
        // return "file:"+System.getProperty("user.dir")+"/"+filename;
    }
    
    
    // initializes factories and transformers
    private void initializeTransformers() throws TransformerConfigurationException {
        // init factories
        if (spf == null) {
            spf = SAXParserFactory.newInstance(); 
            spf.setNamespaceAware(true);
        }
        if (stf == null) {
            stf = (SAXTransformerFactory) TransformerFactory.newInstance();
            try { // try setting indent at factory level
                stf.setAttribute("indent-number", new Integer(2));
            } catch (IllegalArgumentException exc) {} // ignore
        }
        // set up transformer with indenting
        // nb: with some JVMs (eg JDK 1.4.1 on Windows), 
        //     the transformer needs to be reinitialized each time, in order to 
        //     run multiple :r FN commands in tccg 
        if (transformer == null) {
            transformer = stf.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            try { // also try setting indent as a xalan property 
                transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "2");
            } catch (IllegalArgumentException exc) {} // ignore
        }
        // set up apml transformer 
        if (apmlTransformer == null) {
            InputStream toApmlStr = ClassLoader.getSystemResourceAsStream("grammar/to-apml.xsl");
            apmlTransformer = stf.newTransformer(new StreamSource(toApmlStr));
            // nb: DOCTYPE SYSTEM also specified in to-apml.xsl; including  
            //     redundant specification here to workaround omission of DOCTYPE with Linux 1.5 JVM
            apmlTransformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, "apml.dtd");
        }
    }
    
    
    // does setup for LF from XML transformation, and returns a SAXSource for the given input stream
    // nb: need a new filter chain one for each use (perhaps due to an underyling bug)
    private SAXSource fromXmlSetup(InputStream istream) throws IOException {
        try {
            // initialize transformer
            initializeTransformers();
            // load transformations
            if (fromXmlTemplates == null) {
                fromXmlTemplates = new Templates[fromXmlTransforms.length];
                for (int i = 0; i < fromXmlTemplates.length; i++) {
                    String url = fromXmlTransforms[i].toString();
                    fromXmlTemplates[i] = stf.newTemplates(new StreamSource(url));
                }
            }
            // set up initial reader
            SAXParser parser = spf.newSAXParser();
            XMLReader reader = parser.getXMLReader();
            // set up chain of filters
            XMLFilter[] filters = new XMLFilter[fromXmlTransforms.length];
            for (int i = 0; i < filters.length; i++) {
                // create filter
                filters[i] = stf.newXMLFilter(fromXmlTemplates[i]);
                // set parent
                if (i == 0) { filters[0].setParent(reader); }
                else { filters[i].setParent(filters[i-1]); }
            }
            // set final reader/filter
            XMLReader finalReader = (filters.length == 0) ? reader : filters[filters.length-1];
            // set up and return LF from XML SAX source with final reader/filter
            return new SAXSource(finalReader, new InputSource(istream));
        } catch (ParserConfigurationException pce) {
            throw (IOException) new IOException().initCause(pce);
        } catch (SAXException se) {
            throw (IOException) new IOException().initCause(se);
        } catch (TransformerConfigurationException tce) {
            throw (IOException) new IOException().initCause(tce);
        }
    }
    
    /**
     * Loads a document from the XML in the given input stream, 
     * applying the configured from-XML transformations.
     */
    public synchronized Document loadFromXml(InputStream istream) throws IOException {
        try {
            // do setup and get source
            Source source = fromXmlSetup(istream);
            // do transformation
            JDOMResult result = new JDOMResult();
            transformer.transform(source, result);
            // return result doc
            return result.getDocument();
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
    }
    
    /**
     * Loads a document from the XML file with the given filename, 
     * applying the configured from-XML transformations.
     */
    public synchronized Document loadFromXml(String filename) throws IOException {
        BufferedInputStream bis = new BufferedInputStream(new FileInputStream(filename));
        Document retval = loadFromXml(bis);
        bis.close();
        return retval;
    }
    

    // does setup for LF to XML transformation, and returns a SAXSource for the given source
    // nb: need a new filter chain one for each use (perhaps due to an underyling bug)
    private SAXSource toXmlSetup(Source source) throws IOException {
        try {
            // initialize transformer
            initializeTransformers();
            // load transformations
            if (toXmlTemplates == null) {
                toXmlTemplates = new Templates[toXmlTransforms.length];
                for (int i = 0; i < toXmlTemplates.length; i++) {
                    // File file = new File(toXmlTransforms[i]);
                    // toXmlTemplates[i] = stf.newTemplates(new StreamSource(file));
                    String url = toXmlTransforms[i].toString();
                    toXmlTemplates[i] = stf.newTemplates(new StreamSource(url));
                }
            }
            // set up initial reader
            SAXParser parser = spf.newSAXParser();
            XMLReader reader = parser.getXMLReader();
            // set up chain of filters
            XMLFilter[] filters = new XMLFilter[toXmlTransforms.length];
            for (int i = 0; i < filters.length; i++) {
                // create filter
                filters[i] = stf.newXMLFilter(toXmlTemplates[i]);
                // set parent
                if (i == 0) { filters[0].setParent(reader); }
                else { filters[i].setParent(filters[i-1]); }
            }
            // set final reader/filter
            XMLReader finalReader = (filters.length == 0) ? reader : filters[filters.length-1];
            // set up and return LF to XML SAX source with final reader/filter
            return new SAXSource(finalReader, SAXSource.sourceToInputSource(source));
        } catch (ParserConfigurationException pce) {
            throw (IOException) new IOException().initCause(pce);
        } catch (SAXException se) {
            throw (IOException) new IOException().initCause(se);
        } catch (TransformerConfigurationException tce) {
            throw (IOException) new IOException().initCause(tce);
        }
    }

    /**
     * Saves the given LF with the given target string to an XML file 
     * with the given filename, applying the configured to-XML
     * transformations.
     */
    public synchronized void saveToXml(LF lf, String target, String filename) throws IOException { 
        // ensure dirs exist for filename
        File file = new File(filename);
        File parent = file.getParentFile();
        if (parent != null && !parent.exists()) { parent.mkdirs(); }
        FileOutputStream out = new FileOutputStream(file); 
        saveToXml(lf, target, out);
        out.close();
    }

    /**
     * Saves the given LF with the given target string as XML to the 
     * given output stream, applying the configured to-XML
     * transformations.
     */
    public synchronized void saveToXml(LF lf, String target, OutputStream out) throws IOException { 
        // make doc with XML for LF and target
        Document doc = new Document();
        Element root = new Element("xml");
        doc.setRootElement(root);
        root.addContent(HyloHelper.toXml(lf));
        Element targetElt = new Element("target");
        targetElt.addContent(target);
        root.addContent(targetElt);

        // write transformed doc to file
        try {
            // do setup and get source
            Source source = toXmlSetup(new JDOMSource(doc));
            // do transformation
            transformer.transform(source, new StreamResult(new OutputStreamWriter(out)));
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
    }
    
    /**
     * Convenience method to serialize XML.
     */
    public synchronized void serializeXml(Document doc, OutputStream out) throws IOException {
        try {
            initializeTransformers();
            JDOMResult result = new JDOMResult(); // as suggested by Amy Isard, for better java/xml version compatibility
            transformer.transform(new JDOMSource(doc), result);
            XMLOutputter outputter = new XMLOutputter();
            outputter.output(result.getDocument(), new OutputStreamWriter(out)); // end of A.I. suggestion
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
    }

    
    /** 
     * Makes an element for the given LF, applying the configured to-XML transformations.
     */
    public synchronized Element makeLfElt(LF lf) throws IOException { 
        // make doc with LF in it
        Document lfDoc = new Document();
        lfDoc.setRootElement(HyloHelper.toXml(lf));
        // apply to-XML transformations
        try {
            // do setup and get source
            Source source = toXmlSetup(new JDOMSource(lfDoc));
            // do transformation and get resulting doc
            JDOMResult result = new JDOMResult();
            transformer.transform(source, result);
            lfDoc = result.getDocument();
        } catch (TransformerException exc) { 
            throw (IOException) new IOException().initCause(exc);
        }
        return lfDoc.detachRootElement();
    }

    
    /** 
     * Returns whether the given string is a recognized pitch accent.
     */
    public static boolean isPitchAccent(String s) {
        if (pitchAccentsSet == null) {
            pitchAccentsSet = new HashSet<String>();
            for (int i = 0; i < pitchAccents.length; i++) {
                pitchAccentsSet.add(pitchAccents[i]);
            }
        }
        return pitchAccentsSet.contains(s);
    }
    
    /** 
     * Returns whether the given string is a recognized boundary tone. 
     */
    public static boolean isBoundaryTone(String s) {
        if (boundaryTonesSet == null) {
            boundaryTonesSet = new HashSet<String>();
            for (int i = 0; i < boundaryTones.length; i++) {
                boundaryTonesSet.add(boundaryTones[i]);
            }
        }
        return boundaryTonesSet.contains(s);
    }

	/**
	* Returns the name of the loaded grammar. Null if no name given.
	*/
	public final String getName() {
		return grammarName;
	}
}

