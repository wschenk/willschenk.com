function appComponent() {
    return {
        loading: false,
        loginopen: false,
        profile: {},
        loadProfile(): {
            loadProfile( (profile) => setProfile( profile ), (error) => this.loginopen = true );
        }
        setProfile( profile ) { this.profile = profile; this.loginopen = false; }
    }
}
