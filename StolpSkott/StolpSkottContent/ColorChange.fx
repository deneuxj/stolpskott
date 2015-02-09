float4 OldColor;
float4 NewColor;

float4 OldColor2;
float4 NewColor2;

texture SpriteSheet;

sampler SpriteSampler = sampler_state
{
	Texture = < SpriteSheet > ;
};


float4 PixelShaderFunction(float2 SpritePosition : TEXCOORD) : COLOR
{
	float4 color = tex2D(SpriteSampler, SpritePosition);
	if (!any(color - OldColor)) {
		color = NewColor;
	}
	if (!any(color - OldColor2)) {
		color = NewColor2;
	}
	return color;
}

technique Technique1
{
    pass Pass1
    {
        PixelShader = compile ps_2_0 PixelShaderFunction();
    }
}
