#include <gvc.h>

#define N_NODES 3

int main()
{
    Agraph_t *g;
    Agnode_t *n, *m;
    Agedge_t *e;
    GVC_t *gvc;

    // Agnode_t *nodes[N_NODES];
    // char* names[] = {"sam", "fran", "pan"};

    /* set up a graphviz context */
    gvc = gvContext();
    /* Create a simple digraph */
    g = agopen("g", Agdirected, 0);

    // for(size_t i=0; i < N_NODES; i++) {
        // nodes[i] = agnode(g, names[i], 1);
    // }


    n = agnode(g, "n", 1);
    m = agnode(g, "m", 1);
    e = agedge(g, n, m, 0, 1);
    /* Set an attribute - in this case one that affects the visible rendering */
    agsafeset(n, "color", "red", "");
    /* Compute a layout using layout engine */
    gvLayout(gvc, g, "dot");
    /* Write the graph */
    gvRenderFilename(gvc, g, "png", "out.png");
    /* Free layout data */
    gvFreeLayout(gvc, g);
    /* Free graph structures */
    agclose(g);
    /* close output file, free context, and return number of errors */
    return (gvFreeContext(gvc));
}
