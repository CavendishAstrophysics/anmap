/*
 * Include file for the image catalogue and data IO routines of Anmap
 *
 * 
 *  Paul Alexander     3/3/95
 *
 */


/*
 * Define contants used in the catalogue/image IO system
 *
 */

#define HASH_SIZE 20		/* size of the fixed hash table */
#define HEADER_SIZE 2048	/* size of the fixed header record */
/*
 * Structure to implement an image locator vector
 */
typedef struct img_vect {
	int x;
	int y;
	int z;
	int t;
	int v;
	int xr;
	int yr;
	int zr;
	int tr;
	int vr;
} img_vect;

/*
 * Structure to implement an image pixel floating-point vector
 */
typedef struct img_vectF {
	float x;
	float y;
	float z;
	float t;
	int v;
} img_vectF;

/*
 * Structure to implement an image pixel integer vector
 */
typedef struct img_vectI {
	int x;
	int y;
	int z;
	int t;
	int v;
} img_vectI;

/*
 * List structures of vectors
 */
typedef struct img_vectList {
	img_vect v;
	struct img_vectList *vp;
} img_vectList;

/*
 * List structures of general type
 */
typedef struct ida_nbList {
	int    type;
	int    num;
	struct ida_nbList *p;
} ida_nbList;

typedef struct ida_nList {
	int    n;
	struct ida_nList *p;
} ida_nList;

/*
 * Structures used in image classification
 */
typedef struct img_reg {
	float  x1;
	float  x2;
	float  xs;
	float  ys;
	float  zs;
	float  xs2;
	float  ys2;
	float  zs2;
	int    num;
	ida_nList  *p;
	int    anum;
	ida_nbList *ap;
} img_reg;

typedef struct img_class {
	float    min;
	float    max;
	float    incr;
	int      nreg;
	img_reg  *reg;
} img_class;

/*
 * Define the basic structure used to access the image catalogue records
 */
typedef struct ic_iclRec {
	int ce;
	char file[96];
	char source[24];
	char program[16];
	int u1;
	int u2;
	int v1;
	int v2;
	int xdim;
	int ydim;
	int dtype;
	float blank;
	int data;
	int access;
	int open;
	int unit;
	int type;
	int page;
} ic_iclRec;

/*
 * Structure used to implement an image definition record
 */
typedef struct img_defn {
	int ndims;
	int ndata;
	int xdim;
	int ydim;
	int zdim;
	int tdim;
	int vdim;
	int x1;
	int x2;
	int y1;
	int y2;
	int z1;
	int z2;
	int t1;
	int t2;
	int v1;
	int v2;
	int dtype;
	float blank;
	float norm;
	float *data_p;
	int *header_p;
} img_defn;

/*
 * Structure used to implement the HASH table for file IO
 */
typedef struct ic_hashRec {
        int mode;
	ic_iclRec rec;
        img_defn defn;
	img_class class;
} ic_hashRec;

/*
 * Create a fixed-sized hash table
 */
ic_hashRec ic_hash[HASH_SIZE];

/*
 * Define error return codes
 */
#define IC_OK 0
#define IC_ERROR 1
#define IC_NOHASH 2

/*
 * Exported functions
 */
extern ida_nList *ida_allocNList( int n, ida_nList *p );
extern ida_nbList *ida_allocNbList( int type, ida_nbList *p );
extern int ic_hashInit(int p);
extern int ic_hashNext(int *p);
extern int ic_hashExists(int p);
extern int ic_imgCreate(img_defn *defn, int *ih);
extern int ic_imgDestroy(int ih);
extern int ic_imgAssign(int ih_from, int ih_to);
extern int ic_imgRead(ic_iclRec *rec, char *file, int type, img_defn *defn, int *ih);
extern int ic_imgWrite(int ih, char *file, int type);
extern int ic_imgFileDefn(char *file, int *type, img_defn *defn);
extern int ic_imgFileRead(char *file, int *type, img_defn *defn);
extern int ic_imgFileWrite(char *file, int *type, img_defn *defn);
extern int ic_parseImid( Tcl_Interp *interp, char *string, int *ih);
extern int ic_resultImid( Tcl_Interp *interp, int ih);
extern int ic_parseSubIm( Tcl_Interp *interp, int start, int argc, char **argv, img_defn *defn);
extern int ic_parseVect( Tcl_Interp *interp, int start, int argc, char **argv, img_vect *vec);
extern int ic_parseVectF( Tcl_Interp *interp, int start, int argc, char **argv, img_vectF *vec);
extern double img_accImg2( double img, double x, double y );
extern double img_accImg2v( double img, double v, double x, double y );
extern double img_intImg2( double img, double x, double y );
extern double img_intImg2v( double img, double v, double x, double y );
extern double img_accImg3( double img, double x, double y, double z );
extern double img_accImg3v( double img, double v, double x, double y, double z  );
extern double img_intImg3( double img, double x, double y, double z  );
extern double img_intImg3v( double img, double v, double x, double y, double z  );
extern int img_VecCheck( img_defn defn, img_vect vec );
extern int img_Vec2Index( img_defn defn, img_vect vec );
extern void img_Index2Vec( img_defn defn, int indx, img_vect *vec );
extern int img_IndexInit( img_defn defn0, img_defn defn, int *ndata, img_vect *vec );
extern int img_IndexNext( img_defn defn0, img_defn defn, img_vect *vec );
extern img_vectList *img_allocVectList( img_vect vec, img_vectList *vp );
