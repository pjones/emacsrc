<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>

  <!-- =================================================================== -->
  <xsl:template match="/">
    <xsl:apply-templates select="/forecast/txt_forecast/forecastday"/>
    <xsl:apply-templates select="/forecast/moon_phase/sunset"/>
  </xsl:template>

  <!-- =================================================================== -->
  <xsl:template match="forecastday">
    <xsl:value-of select="title/text()"/>:<xsl:text> </xsl:text>
    <xsl:value-of select="fcttext/text()"/><xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <xsl:template match="sunset">
    <xsl:text>The sun will set tonight at </xsl:text>
    <xsl:value-of select="concat(hour/text(), ':', minute/text())"/>.
    <xsl:value-of select="fcttext/text()"/><xsl:text>&#10;&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
