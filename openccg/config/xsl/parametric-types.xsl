<?xml version="1.0"?>


<!-- Rules to set up initial types.xml file. Lexicons refer to s-case, a-case, p-case,
     whose values are derived from the parameters.xml
- Cem Bozsahin 2004 (cem.bozsahin@ed.ac.uk / bozsahin@metu.edu.tr)
-->

<xsl:transform 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	version="1.0" 
	xmlns:set="xalan://java.util.HashSet"
	xmlns:xalan2="http://xml.apache.org/xslt"
	exclude-result-prefixes="set xalan2">
  
   <xsl:output indent="yes" xalan2:indent-amount="2"/> 
   <xsl:strip-space elements="*"/>

  <xsl:variable name="infinitive.type" select="//infinitive/@subject-type"/>
  
	  

  <xsl:template match="language">

   <!-- ** start output here ** -->
   
	<xsl:comment> 
      - This file is generated by parametric-types.xsl from parameter specs
	    (parameters.xml) to set up  types.xml file for development.
         
      - If the language in question has eg. quirky subjects etc., add their 
	    values under appropriate types.
	 
	 </xsl:comment>
	 <xsl:comment>
	   subject-case, s-case, p-case and a-case are pre-defined types that are
	   used in the automatically generated initial lexicon (preset-families
	   .xml). We recommend that you dont change them. They handle
	   accusative/ergative mapping among other things (cf. types.xml initial
	   hierarchy).
	  
	 </xsl:comment>

    <types xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
	xsi:noNamespaceSchemaLocation="../types.xsd">
	
      <xsl:attribute name="name">
        <xsl:value-of select="@name"/>
      </xsl:attribute>

      <xsl:apply-templates select="parameters/iv">
        <xsl:with-param name="new.case" select="@type"/>
        <xsl:with-param name="iv.1.case" select="parameters/iv//@case"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="parameters/tv">
        <xsl:with-param name="new.case" select="@type"/>
        <xsl:with-param name="iv.1.case" select="parameters/iv//@case"/>
      </xsl:apply-templates>
	
    </types>

  </xsl:template>

  <xsl:template match="parameters/iv">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>


    <xsl:apply-templates select="setarg|arg">
      <xsl:with-param name="new.case" select="$new.case"/>
      <xsl:with-param name="iv.1.case" select="$iv.1.case"/>
    </xsl:apply-templates>

  </xsl:template>

  <xsl:template match="parameters/tv">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>

    <xsl:apply-templates select="setarg|arg">
      <xsl:with-param name="mode" select="'normal'"/>
      <xsl:with-param name="new.case" select="$new.case"/>
      <xsl:with-param name="iv.1.case" select="$iv.1.case"/>
    </xsl:apply-templates>

  </xsl:template>


  <xsl:template match="setarg">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>

    <xsl:apply-templates select="s-argument|a-argument|p-argument">
      <xsl:with-param name="new.case" select="$new.case"/>
      <xsl:with-param name="iv.1.case" select="$iv.1.case"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="arg">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>

    <xsl:apply-templates select="s-argument|a-argument|p-argument">
      <xsl:with-param name="new.case" select="$new.case"/>
      <xsl:with-param name="iv.1.case" select="$iv.1.case"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="a-argument">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>

    <xsl:choose>
      <xsl:when test="$new.case = 'erg'">
        <type parents="a-case">
          <xsl:attribute name="name">
            <xsl:value-of select="$new.case"/>
          </xsl:attribute>
        </type>
        <type name="a-case"/>
      </xsl:when>
      <xsl:when test="$new.case = 'acc'">
        <type parents="a-case s-case">
          <xsl:attribute name="name">
            <xsl:value-of select="$iv.1.case"/>
          </xsl:attribute>
        </type>
        <type parents="subject-case" name="a-case"/>
      </xsl:when>

      <xsl:otherwise>   <!-- unaligned language -->
        <type parents="a-case">
          <xsl:attribute name="name">
            <xsl:value-of select="'unknown'"/>
          </xsl:attribute>
        </type>
        <type name="a-case"/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template match="p-argument">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>

    <xsl:choose>
      <xsl:when test="$new.case = 'erg'">
        <type parents="s-case p-case">
          <xsl:attribute name="name">
            <xsl:value-of select="$iv.1.case"/>
          </xsl:attribute>
        </type>
        <type parents="subject-case" name="p-case"/>
      </xsl:when>
      <xsl:when test="$new.case = 'acc'">
        <type parents="p-case">
          <xsl:attribute name="name">
            <xsl:value-of select="$new.case"/>
          </xsl:attribute>
        </type>
        <type name="p-case"/>
      </xsl:when>

      <xsl:otherwise>   <!-- unaligned language -->
        <type parents="p-case">
          <xsl:attribute name="name">
            <xsl:value-of select="'unknown2'"/>
          </xsl:attribute>
        </type>
        <type name="p-case"/>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>


  <xsl:template match="s-argument">
    <xsl:param name="new.case"/>
    <xsl:param name="iv.1.case"/>

    <type parents="subject-case" name="s-case"/>
    <type name="subject-case"/>
    <xsl:choose>
      <xsl:when test="$new.case = 'erg'"/>
      <xsl:when test="$new.case = 'acc'"/>
      <xsl:otherwise>   <!-- unaligned language -->
        <type parents="s-case">
          <xsl:attribute name="name">
            <xsl:value-of select="$iv.1.case"/>
          </xsl:attribute>
        </type>
        <xsl:comment>
		 Warning: Please substitute case names as children
		of a-case and p-case since these cannot be predicted parametrically
		for an unaligned language (cf. unknown and unknown2 values)
        </xsl:comment>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:transform>
