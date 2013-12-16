function y = twoscoverflow(x, integerbits);

y = mod(x - 2^(integerbits - 1), 2^integerbits) - 2^(integerbits - 1);
