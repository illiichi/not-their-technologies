void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = fragCoord/iResolution.xy;
    uv.y *= 3.0;
    uv.x *= 8.0;
    uv.y -= 0.67;
    uv.x -= 1.;
    vec3 col = vec3(0.);
    float num = 10.;
    for(float i = 1.0; i < num; i++){
        float c = sin(3.3 * uv.y * float(i)) * sin( uv.x * sin(0.2 * uv.y));
        float shape =  smoothstep(1.2 - clamp(distance(c + uv.y, uv.x) * (0.93 + 0.9 * uv.x * uv.y), 0.0, 1.0), 1.0, 0.9);
        if (shape > 0.9) {
           col += vec3(252. / 255., 203. / 255., 203. / 255.);
        } else {
           col += vec3(131. / 255.  + 1. * uv.y * uv.x, 151. / 255., 181. / 255.);
        }
    }
	fragColor = vec4(col / num,1.0);
}