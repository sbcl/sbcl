<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

<xsl:import href="docbook_chunk.xsl"/>
<xsl:param name="use.id.as.filename" select="1"/>

<!-- Force TT HTML markup for <type> DocBook markup -->
<xsl:template match="type">
<tt xmlns="http://www.w3.org/1999/xhtml" class="type">
  <xsl:value-of select="."/>
</tt>
</xsl:template>

</xsl:stylesheet>

